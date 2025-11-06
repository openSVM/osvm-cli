//! Basic I/O operations for OVSM
//!
//! This module implements Common Lisp's basic I/O functions including:
//! - Output functions (PRINT, PRIN1, PRINC, etc.)
//! - Input functions (READ, READ-LINE, READ-CHAR, etc.)
//! - File operations (OPEN, CLOSE, WITH-OPEN-FILE, etc.)
//! - String streams (WITH-OUTPUT-TO-STRING, WITH-INPUT-FROM-STRING)
//!
//! Implementation notes:
//! - File operations use Rust's std::fs and std::io
//! - Stream abstractions are simplified for OVSM's use case
//! - READ functions parse OVSM LISP syntax
//! - All operations return Result for error handling

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

// ============================================================================
// OUTPUT FUNCTIONS (9 functions)
// ============================================================================

/// PRINT - Print value with newline to stdout
pub struct PrintTool;

impl Tool for PrintTool {
    fn name(&self) -> &str {
        "PRINT"
    }

    fn description(&self) -> &str {
        "Print value with newline to stdout"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PRINT".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        // Print all arguments separated by spaces
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", arg);
        }
        println!();

        // Return the first argument (Common Lisp convention)
        Ok(args[0].clone())
    }
}

/// PRIN1 - Print readable representation (with escape sequences)
pub struct Prin1Tool;

impl Tool for Prin1Tool {
    fn name(&self) -> &str {
        "PRIN1"
    }

    fn description(&self) -> &str {
        "Print readable representation with escape sequences"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PRIN1".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        // Print with debug formatting (shows escape sequences)
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{:?}", arg);
        }

        Ok(args[0].clone())
    }
}

/// PRINC - Print without escape characters
pub struct PrincTool;

impl Tool for PrincTool {
    fn name(&self) -> &str {
        "PRINC"
    }

    fn description(&self) -> &str {
        "Print without escape characters"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PRINC".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        // Print plain display format
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", arg);
        }

        Ok(args[0].clone())
    }
}

/// PPRINT - Pretty print (same as PRINT for now)
pub struct PprintTool;

impl Tool for PprintTool {
    fn name(&self) -> &str {
        "PPRINT"
    }

    fn description(&self) -> &str {
        "Pretty print value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PPRINT".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        // For now, same as PRINT (future: could add indentation)
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                print!(" ");
            }
            print!("{}", arg);
        }
        println!();

        Ok(args[0].clone())
    }
}

/// WRITE - General write function
pub struct WriteTool;

impl Tool for WriteTool {
    fn name(&self) -> &str {
        "WRITE"
    }

    fn description(&self) -> &str {
        "General write function"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WRITE".to_string(),
                reason: "Expected at least one argument".to_string(),
            });
        }

        // Print with display format
        print!("{}", args[0]);

        Ok(args[0].clone())
    }
}

/// WRITE-LINE - Write string with newline
pub struct WriteLineTool;

impl Tool for WriteLineTool {
    fn name(&self) -> &str {
        "WRITE-LINE"
    }

    fn description(&self) -> &str {
        "Write string with newline"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WRITE-LINE".to_string(),
                reason: "Expected a string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        println!("{}", s);

        Ok(args[0].clone())
    }
}

/// WRITE-STRING - Write string without newline
pub struct WriteStringTool;

impl Tool for WriteStringTool {
    fn name(&self) -> &str {
        "WRITE-STRING"
    }

    fn description(&self) -> &str {
        "Write string without newline"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WRITE-STRING".to_string(),
                reason: "Expected a string argument".to_string(),
            });
        }

        let s = args[0].as_string()?;
        print!("{}", s);

        Ok(args[0].clone())
    }
}

/// TERPRI - Output newline
pub struct TerpriTool;

impl Tool for TerpriTool {
    fn name(&self) -> &str {
        "TERPRI"
    }

    fn description(&self) -> &str {
        "Output newline"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        println!();
        Ok(Value::Null)
    }
}

/// FRESH-LINE - Output newline if needed (always outputs for simplicity)
pub struct FreshLineTool;

impl Tool for FreshLineTool {
    fn name(&self) -> &str {
        "FRESH-LINE"
    }

    fn description(&self) -> &str {
        "Output newline if needed"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // For simplicity, always output newline
        // (proper implementation would track cursor position)
        println!();
        Ok(Value::Bool(true))
    }
}

// ============================================================================
// INPUT FUNCTIONS (4 functions)
// ============================================================================

/// READ - Read S-expression from string
pub struct ReadTool;

impl Tool for ReadTool {
    fn name(&self) -> &str {
        "READ"
    }

    fn description(&self) -> &str {
        "Read S-expression from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ".to_string(),
                reason: "Expected a string to read from".to_string(),
            });
        }

        let input = args[0].as_string()?;

        // For now, just return the trimmed string
        // A full implementation would parse and evaluate the S-expression
        Ok(Value::String(input.trim().to_string()))
    }
}

/// READ-LINE - Read line of text from string
pub struct ReadLineTool;

impl Tool for ReadLineTool {
    fn name(&self) -> &str {
        "READ-LINE"
    }

    fn description(&self) -> &str {
        "Read line of text from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ-LINE".to_string(),
                reason: "Expected a string to read from".to_string(),
            });
        }

        let input = args[0].as_string()?;

        // Find first newline or take whole string
        let line = input.lines().next().unwrap_or("").to_string();

        Ok(Value::String(line))
    }
}

/// READ-CHAR - Read single character from string
pub struct ReadCharTool;

impl Tool for ReadCharTool {
    fn name(&self) -> &str {
        "READ-CHAR"
    }

    fn description(&self) -> &str {
        "Read single character from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ-CHAR".to_string(),
                reason: "Expected a string to read from".to_string(),
            });
        }

        let input = args[0].as_string()?;

        // Get first character
        let ch = input
            .chars()
            .next()
            .ok_or_else(|| Error::InvalidArguments {
                tool: "READ-CHAR".to_string(),
                reason: "String is empty".to_string(),
            })?;

        Ok(Value::String(ch.to_string()))
    }
}

/// READ-FROM-STRING - Read S-expression from string (alias of READ)
pub struct ReadFromStringTool;

impl Tool for ReadFromStringTool {
    fn name(&self) -> &str {
        "READ-FROM-STRING"
    }

    fn description(&self) -> &str {
        "Read S-expression from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Same as READ
        ReadTool.execute(args)
    }
}

// ============================================================================
// FILE OPERATIONS (5 functions)
// ============================================================================

/// WITH-OPEN-FILE - Open file with auto-close (macro-like function)
pub struct WithOpenFileTool;

impl Tool for WithOpenFileTool {
    fn name(&self) -> &str {
        "WITH-OPEN-FILE"
    }

    fn description(&self) -> &str {
        "Open file and execute operations, auto-closing afterwards"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "WITH-OPEN-FILE".to_string(),
                reason: "Expected (filename mode) and body".to_string(),
            });
        }

        let filename = args[0].as_string()?;
        let mode = if args.len() > 1 {
            args[1].as_string()?
        } else {
            "r"
        };

        // Open file based on mode
        let content = match mode {
            "r" | "read" => {
                // Read mode
                let content =
                    std::fs::read_to_string(filename).map_err(|e| Error::InvalidArguments {
                        tool: "WITH-OPEN-FILE".to_string(),
                        reason: format!("Failed to read file: {}", e),
                    })?;
                Ok(Value::String(content))
            }
            "w" | "write" => {
                // Write mode - would need body to execute
                Ok(Value::Null)
            }
            _ => Err(Error::InvalidArguments {
                tool: "WITH-OPEN-FILE".to_string(),
                reason: format!("Unknown mode: {}", mode),
            }),
        }?;

        Ok(content)
    }
}

/// OPEN - Open file/stream
pub struct OpenTool;

impl Tool for OpenTool {
    fn name(&self) -> &str {
        "OPEN"
    }

    fn description(&self) -> &str {
        "Open file and return its contents"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "OPEN".to_string(),
                reason: "Expected filename".to_string(),
            });
        }

        let filename = args[0].as_string()?;

        // Read file contents
        let content = std::fs::read_to_string(filename).map_err(|e| Error::InvalidArguments {
            tool: "OPEN".to_string(),
            reason: format!("Failed to open file: {}", e),
        })?;

        Ok(Value::String(content))
    }
}

/// CLOSE - Close stream (no-op in OVSM since we don't have persistent streams)
pub struct CloseTool;

impl Tool for CloseTool {
    fn name(&self) -> &str {
        "CLOSE"
    }

    fn description(&self) -> &str {
        "Close stream (no-op in OVSM)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // In OVSM, we don't have persistent stream objects
        // Files are automatically closed after reading
        Ok(Value::Bool(true))
    }
}

/// FILE-POSITION - Get file position (returns file size for now)
pub struct FilePositionTool;

impl Tool for FilePositionTool {
    fn name(&self) -> &str {
        "FILE-POSITION"
    }

    fn description(&self) -> &str {
        "Get file position (returns file size)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FILE-POSITION".to_string(),
                reason: "Expected filename".to_string(),
            });
        }

        let filename = args[0].as_string()?;

        // Get file size
        let metadata = std::fs::metadata(filename).map_err(|e| Error::InvalidArguments {
            tool: "FILE-POSITION".to_string(),
            reason: format!("Failed to get file info: {}", e),
        })?;

        Ok(Value::Int(metadata.len() as i64))
    }
}

/// FILE-LENGTH - Get file length
pub struct FileLengthTool;

impl Tool for FileLengthTool {
    fn name(&self) -> &str {
        "FILE-LENGTH"
    }

    fn description(&self) -> &str {
        "Get file length in bytes"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FILE-LENGTH".to_string(),
                reason: "Expected filename".to_string(),
            });
        }

        let filename = args[0].as_string()?;

        // Get file size
        let metadata = std::fs::metadata(filename).map_err(|e| Error::InvalidArguments {
            tool: "FILE-LENGTH".to_string(),
            reason: format!("Failed to get file info: {}", e),
        })?;

        Ok(Value::Int(metadata.len() as i64))
    }
}

// ============================================================================
// STRING STREAMS (2 functions)
// ============================================================================

/// WITH-OUTPUT-TO-STRING - Create string output stream
pub struct WithOutputToStringTool;

impl Tool for WithOutputToStringTool {
    fn name(&self) -> &str {
        "WITH-OUTPUT-TO-STRING"
    }

    fn description(&self) -> &str {
        "Collect output into a string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // In OVSM, this would collect all arguments into a string
        let mut result = String::new();

        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                result.push(' ');
            }
            result.push_str(&arg.to_string());
        }

        Ok(Value::String(result))
    }
}

/// WITH-INPUT-FROM-STRING - Create string input stream
pub struct WithInputFromStringTool;

impl Tool for WithInputFromStringTool {
    fn name(&self) -> &str {
        "WITH-INPUT-FROM-STRING"
    }

    fn description(&self) -> &str {
        "Create input stream from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WITH-INPUT-FROM-STRING".to_string(),
                reason: "Expected a string".to_string(),
            });
        }

        // Simply return the string - it can be used with READ
        Ok(args[0].clone())
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all I/O basic tools
pub fn register(registry: &mut ToolRegistry) {
    // Output functions
    registry.register(PrintTool);
    registry.register(Prin1Tool);
    registry.register(PrincTool);
    registry.register(PprintTool);
    registry.register(WriteTool);
    registry.register(WriteLineTool);
    registry.register(WriteStringTool);
    registry.register(TerpriTool);
    registry.register(FreshLineTool);

    // Input functions
    registry.register(ReadTool);
    registry.register(ReadLineTool);
    registry.register(ReadCharTool);
    registry.register(ReadFromStringTool);

    // File operations
    registry.register(WithOpenFileTool);
    registry.register(OpenTool);
    registry.register(CloseTool);
    registry.register(FilePositionTool);
    registry.register(FileLengthTool);

    // String streams
    registry.register(WithOutputToStringTool);
    registry.register(WithInputFromStringTool);
}
