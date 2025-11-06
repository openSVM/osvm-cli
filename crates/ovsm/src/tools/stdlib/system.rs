//! System functions for OVSM
//!
//! Environment variables, process management, system information, and file system operations.
//! Provides Common Lisp-style system interaction capabilities.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;
use std::env;
use std::sync::Arc;

// System functions (35 total)

// ============================================================
// ENVIRONMENT VARIABLES
// ============================================================

/// GETENV - Get environment variable
pub struct GetenvTool;
impl Tool for GetenvTool {
    fn name(&self) -> &str {
        "GETENV"
    }
    fn description(&self) -> &str {
        "Get environment variable value"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "GETENV requires variable name".to_string(),
            });
        }
        match &args[0] {
            Value::String(var_name) => match env::var(var_name) {
                Ok(value) => Ok(Value::String(value)),
                Err(_) => Ok(Value::Null),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// SETENV - Set environment variable
pub struct SetenvTool;
impl Tool for SetenvTool {
    fn name(&self) -> &str {
        "SETENV"
    }
    fn description(&self) -> &str {
        "Set environment variable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "SETENV requires name and value".to_string(),
            });
        }
        match (&args[0], &args[1]) {
            (Value::String(name), Value::String(value)) => {
                env::set_var(name, value);
                Ok(Value::Bool(true))
            }
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// UNSETENV - Unset environment variable
pub struct UnsetenvTool;
impl Tool for UnsetenvTool {
    fn name(&self) -> &str {
        "UNSETENV"
    }
    fn description(&self) -> &str {
        "Unset environment variable"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "UNSETENV requires variable name".to_string(),
            });
        }
        match &args[0] {
            Value::String(name) => {
                env::remove_var(name);
                Ok(Value::Bool(true))
            }
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// ENVIRONMENT - Get all environment variables
pub struct EnvironmentTool;
impl Tool for EnvironmentTool {
    fn name(&self) -> &str {
        "ENVIRONMENT"
    }
    fn description(&self) -> &str {
        "Get all environment variables as object"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        let mut env_map = HashMap::new();
        for (key, value) in env::vars() {
            env_map.insert(key, Value::String(value));
        }
        Ok(Value::Object(Arc::new(env_map)))
    }
}

// ============================================================
// PROCESS MANAGEMENT
// ============================================================

/// EXIT - Exit program
pub struct ExitTool;
impl Tool for ExitTool {
    fn name(&self) -> &str {
        "EXIT"
    }
    fn description(&self) -> &str {
        "Exit program with status code"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let code = if args.is_empty() {
            0
        } else {
            match &args[0] {
                Value::Int(n) => *n as i32,
                _ => 0,
            }
        };
        // In a real implementation, this would call std::process::exit(code)
        // For now, just return the exit code
        Ok(Value::Int(code as i64))
    }
}

/// QUIT - Quit program (alias for EXIT)
pub struct QuitTool;
impl Tool for QuitTool {
    fn name(&self) -> &str {
        "QUIT"
    }
    fn description(&self) -> &str {
        "Quit program (alias for EXIT)"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        ExitTool.execute(args)
    }
}

/// RUN-PROGRAM - Run external program
pub struct RunProgramTool;
impl Tool for RunProgramTool {
    fn name(&self) -> &str {
        "RUN-PROGRAM"
    }
    fn description(&self) -> &str {
        "Run external program with arguments"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "RUN-PROGRAM requires program name".to_string(),
            });
        }
        // Simplified: just return success status
        // Real implementation would use std::process::Command
        Ok(Value::Object(Arc::new({
            let mut result = HashMap::new();
            result.insert("status".to_string(), Value::Int(0));
            result.insert("output".to_string(), Value::String(String::new()));
            result
        })))
    }
}

/// GET-PID - Get current process ID
pub struct GetPidTool;
impl Tool for GetPidTool {
    fn name(&self) -> &str {
        "GET-PID"
    }
    fn description(&self) -> &str {
        "Get current process ID"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Int(std::process::id() as i64))
    }
}

// ============================================================
// SYSTEM INFORMATION
// ============================================================

/// MACHINE-TYPE - Get machine type
pub struct MachineTypeTool;
impl Tool for MachineTypeTool {
    fn name(&self) -> &str {
        "MACHINE-TYPE"
    }
    fn description(&self) -> &str {
        "Get machine type/architecture"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(std::env::consts::ARCH.to_string()))
    }
}

/// MACHINE-VERSION - Get machine version
pub struct MachineVersionTool;
impl Tool for MachineVersionTool {
    fn name(&self) -> &str {
        "MACHINE-VERSION"
    }
    fn description(&self) -> &str {
        "Get machine version"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(format!(
            "{}-{}",
            std::env::consts::ARCH,
            std::env::consts::OS
        )))
    }
}

/// SOFTWARE-TYPE - Get software/OS type
pub struct SoftwareTypeTool;
impl Tool for SoftwareTypeTool {
    fn name(&self) -> &str {
        "SOFTWARE-TYPE"
    }
    fn description(&self) -> &str {
        "Get operating system type"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(std::env::consts::OS.to_string()))
    }
}

/// SOFTWARE-VERSION - Get software/OS version
pub struct SoftwareVersionTool;
impl Tool for SoftwareVersionTool {
    fn name(&self) -> &str {
        "SOFTWARE-VERSION"
    }
    fn description(&self) -> &str {
        "Get operating system version"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(std::env::consts::OS.to_string()))
    }
}

/// LISP-IMPLEMENTATION-TYPE - Get Lisp implementation type
pub struct LispImplementationTypeTool;
impl Tool for LispImplementationTypeTool {
    fn name(&self) -> &str {
        "LISP-IMPLEMENTATION-TYPE"
    }
    fn description(&self) -> &str {
        "Get Lisp implementation type"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("OVSM".to_string()))
    }
}

/// LISP-IMPLEMENTATION-VERSION - Get Lisp implementation version
pub struct LispImplementationVersionTool;
impl Tool for LispImplementationVersionTool {
    fn name(&self) -> &str {
        "LISP-IMPLEMENTATION-VERSION"
    }
    fn description(&self) -> &str {
        "Get Lisp implementation version"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(env!("CARGO_PKG_VERSION").to_string()))
    }
}

/// SHORT-SITE-NAME - Get short site name
pub struct ShortSiteNameTool;
impl Tool for ShortSiteNameTool {
    fn name(&self) -> &str {
        "SHORT-SITE-NAME"
    }
    fn description(&self) -> &str {
        "Get short site name"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(
            env::var("HOSTNAME").unwrap_or_else(|_| "localhost".to_string()),
        ))
    }
}

/// LONG-SITE-NAME - Get long site name
pub struct LongSiteNameTool;
impl Tool for LongSiteNameTool {
    fn name(&self) -> &str {
        "LONG-SITE-NAME"
    }
    fn description(&self) -> &str {
        "Get long site name"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(
            env::var("HOSTNAME").unwrap_or_else(|_| "localhost".to_string()),
        ))
    }
}

// ============================================================
// FILE SYSTEM OPERATIONS
// ============================================================

/// DIRECTORY - List directory contents
pub struct DirectoryTool;
impl Tool for DirectoryTool {
    fn name(&self) -> &str {
        "DIRECTORY"
    }
    fn description(&self) -> &str {
        "List directory contents"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        match &args[0] {
            Value::String(path) => match std::fs::read_dir(path) {
                Ok(entries) => {
                    let files: Vec<Value> = entries
                        .filter_map(|e| e.ok())
                        .map(|e| Value::String(e.path().display().to_string()))
                        .collect();
                    Ok(Value::Array(Arc::new(files)))
                }
                Err(_) => Ok(Value::Array(Arc::new(vec![]))),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// FILE-WRITE-DATE - Get file modification time
pub struct FileWriteDateTool;
impl Tool for FileWriteDateTool {
    fn name(&self) -> &str {
        "FILE-WRITE-DATE"
    }
    fn description(&self) -> &str {
        "Get file modification time"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "FILE-WRITE-DATE requires file path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => match std::fs::metadata(path) {
                Ok(metadata) => match metadata.modified() {
                    Ok(time) => match time.duration_since(std::time::UNIX_EPOCH) {
                        Ok(duration) => Ok(Value::Int(duration.as_secs() as i64)),
                        Err(_) => Ok(Value::Int(0)),
                    },
                    Err(_) => Ok(Value::Int(0)),
                },
                Err(_) => Ok(Value::Null),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// FILE-AUTHOR - Get file author
pub struct FileAuthorTool;
impl Tool for FileAuthorTool {
    fn name(&self) -> &str {
        "FILE-AUTHOR"
    }
    fn description(&self) -> &str {
        "Get file author/owner"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "FILE-AUTHOR requires file path".to_string(),
            });
        }
        // Simplified: return unknown
        Ok(Value::String("unknown".to_string()))
    }
}

/// DELETE-FILE - Delete file
pub struct DeleteFileTool;
impl Tool for DeleteFileTool {
    fn name(&self) -> &str {
        "DELETE-FILE"
    }
    fn description(&self) -> &str {
        "Delete file"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "DELETE-FILE requires file path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => match std::fs::remove_file(path) {
                Ok(_) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// RENAME-FILE - Rename/move file
pub struct RenameFileTool;
impl Tool for RenameFileTool {
    fn name(&self) -> &str {
        "RENAME-FILE"
    }
    fn description(&self) -> &str {
        "Rename or move file"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "RENAME-FILE requires old and new paths".to_string(),
            });
        }
        match (&args[0], &args[1]) {
            (Value::String(old_path), Value::String(new_path)) => {
                match std::fs::rename(old_path, new_path) {
                    Ok(_) => Ok(Value::Bool(true)),
                    Err(_) => Ok(Value::Bool(false)),
                }
            }
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// ENSURE-DIRECTORIES-EXIST - Create directory path
pub struct EnsureDirectoriesExistTool;
impl Tool for EnsureDirectoriesExistTool {
    fn name(&self) -> &str {
        "ENSURE-DIRECTORIES-EXIST"
    }
    fn description(&self) -> &str {
        "Create directory and parent directories"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "ENSURE-DIRECTORIES-EXIST requires path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => match std::fs::create_dir_all(path) {
                Ok(_) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// FILE-EXISTS-P - Check if file exists
pub struct FileExistsPTool;
impl Tool for FileExistsPTool {
    fn name(&self) -> &str {
        "FILE-EXISTS-P"
    }
    fn description(&self) -> &str {
        "Check if file exists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "FILE-EXISTS-P requires path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => Ok(Value::Bool(std::path::Path::new(path).exists())),
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// DIRECTORY-P - Check if path is directory
pub struct DirectoryPTool;
impl Tool for DirectoryPTool {
    fn name(&self) -> &str {
        "DIRECTORY-P"
    }
    fn description(&self) -> &str {
        "Check if path is a directory"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "DIRECTORY-P requires path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => Ok(Value::Bool(std::path::Path::new(path).is_dir())),
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

/// FILE-P - Check if path is file
pub struct FilePTool;
impl Tool for FilePTool {
    fn name(&self) -> &str {
        "FILE-P"
    }
    fn description(&self) -> &str {
        "Check if path is a file"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "FILE-P requires path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => Ok(Value::Bool(std::path::Path::new(path).is_file())),
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

// ============================================================
// WORKING DIRECTORY
// ============================================================

/// GET-WORKING-DIRECTORY - Get current working directory
pub struct GetWorkingDirectoryTool;
impl Tool for GetWorkingDirectoryTool {
    fn name(&self) -> &str {
        "GET-WORKING-DIRECTORY"
    }
    fn description(&self) -> &str {
        "Get current working directory"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        match env::current_dir() {
            Ok(path) => Ok(Value::String(path.display().to_string())),
            Err(_) => Ok(Value::String(".".to_string())),
        }
    }
}

/// SET-WORKING-DIRECTORY - Set current working directory
pub struct SetWorkingDirectoryTool;
impl Tool for SetWorkingDirectoryTool {
    fn name(&self) -> &str {
        "SET-WORKING-DIRECTORY"
    }
    fn description(&self) -> &str {
        "Set current working directory"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "UNKNOWN".to_string(),
                reason: "SET-WORKING-DIRECTORY requires path".to_string(),
            });
        }
        match &args[0] {
            Value::String(path) => match env::set_current_dir(path) {
                Ok(_) => Ok(Value::Bool(true)),
                Err(_) => Ok(Value::Bool(false)),
            },
            _ => Err(Error::TypeError {
                expected: "valid argument".to_string(),
                got: "invalid".to_string(),
            }),
        }
    }
}

// ============================================================
// TIME FUNCTIONS
// ============================================================

/// GET-UNIVERSAL-TIME - Get universal time
pub struct GetUniversalTimeTool;
impl Tool for GetUniversalTimeTool {
    fn name(&self) -> &str {
        "GET-UNIVERSAL-TIME"
    }
    fn description(&self) -> &str {
        "Get universal time (seconds since epoch)"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => Ok(Value::Int(duration.as_secs() as i64)),
            Err(_) => Ok(Value::Int(0)),
        }
    }
}

/// GET-DECODED-TIME - Get decoded time
pub struct GetDecodedTimeTool;
impl Tool for GetDecodedTimeTool {
    fn name(&self) -> &str {
        "GET-DECODED-TIME"
    }
    fn description(&self) -> &str {
        "Get decoded time as object"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Simplified: return object with timestamp
        let mut time_obj = HashMap::new();
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => {
                time_obj.insert(
                    "timestamp".to_string(),
                    Value::Int(duration.as_secs() as i64),
                );
            }
            Err(_) => {
                time_obj.insert("timestamp".to_string(), Value::Int(0));
            }
        }
        Ok(Value::Object(Arc::new(time_obj)))
    }
}

/// SLEEP - Sleep for specified seconds
pub struct SleepTool;
impl Tool for SleepTool {
    fn name(&self) -> &str {
        "SLEEP"
    }
    fn description(&self) -> &str {
        "Sleep for specified seconds"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        match &args[0] {
            Value::Int(n) if *n > 0 => {
                std::thread::sleep(std::time::Duration::from_secs(*n as u64));
                Ok(Value::Null)
            }
            Value::Float(f) if *f > 0.0 => {
                std::thread::sleep(std::time::Duration::from_secs_f64(*f));
                Ok(Value::Null)
            }
            _ => Ok(Value::Null),
        }
    }
}

/// GET-INTERNAL-REAL-TIME - Get internal real time
pub struct GetInternalRealTimeTool;
impl Tool for GetInternalRealTimeTool {
    fn name(&self) -> &str {
        "GET-INTERNAL-REAL-TIME"
    }
    fn description(&self) -> &str {
        "Get internal real time in milliseconds"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        match std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH) {
            Ok(duration) => Ok(Value::Int(duration.as_millis() as i64)),
            Err(_) => Ok(Value::Int(0)),
        }
    }
}

/// GET-INTERNAL-RUN-TIME - Get internal run time
pub struct GetInternalRunTimeTool;
impl Tool for GetInternalRunTimeTool {
    fn name(&self) -> &str {
        "GET-INTERNAL-RUN-TIME"
    }
    fn description(&self) -> &str {
        "Get internal run time in milliseconds"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Simplified: same as real time
        GetInternalRealTimeTool.execute(_args)
    }
}

/// Register all system functions
pub fn register(registry: &mut ToolRegistry) {
    // Environment variables
    registry.register(GetenvTool);
    registry.register(SetenvTool);
    registry.register(UnsetenvTool);
    registry.register(EnvironmentTool);

    // Process management
    registry.register(ExitTool);
    registry.register(QuitTool);
    registry.register(RunProgramTool);
    registry.register(GetPidTool);

    // System information
    registry.register(MachineTypeTool);
    registry.register(MachineVersionTool);
    registry.register(SoftwareTypeTool);
    registry.register(SoftwareVersionTool);
    registry.register(LispImplementationTypeTool);
    registry.register(LispImplementationVersionTool);
    registry.register(ShortSiteNameTool);
    registry.register(LongSiteNameTool);

    // File system operations
    registry.register(DirectoryTool);
    registry.register(FileWriteDateTool);
    registry.register(FileAuthorTool);
    registry.register(DeleteFileTool);
    registry.register(RenameFileTool);
    registry.register(EnsureDirectoriesExistTool);
    registry.register(FileExistsPTool);
    registry.register(DirectoryPTool);
    registry.register(FilePTool);

    // Working directory
    registry.register(GetWorkingDirectoryTool);
    registry.register(SetWorkingDirectoryTool);

    // Time functions
    registry.register(GetUniversalTimeTool);
    registry.register(GetDecodedTimeTool);
    registry.register(SleepTool);
    registry.register(GetInternalRealTimeTool);
    registry.register(GetInternalRunTimeTool);
}
