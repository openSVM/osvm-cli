//! Stream operations for OVSM
//!
//! Simplified stream implementation for OVSM. Unlike full Common Lisp streams,
//! OVSM streams are lightweight wrappers around strings and data structures.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

// ============================================================================
// STREAM CREATION (3 functions)
// ============================================================================

/// MAKE-STRING-INPUT-STREAM - Create input stream from string
pub struct MakeStringInputStreamTool;

impl Tool for MakeStringInputStreamTool {
    fn name(&self) -> &str {
        "MAKE-STRING-INPUT-STREAM"
    }

    fn description(&self) -> &str {
        "Create input stream from string"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-STRING-INPUT-STREAM".to_string(),
                reason: "Expected string argument".to_string(),
            });
        }

        // In OVSM, a "stream" is just a string
        Ok(args[0].clone())
    }
}

/// MAKE-STRING-OUTPUT-STREAM - Create output stream
pub struct MakeStringOutputStreamTool;

impl Tool for MakeStringOutputStreamTool {
    fn name(&self) -> &str {
        "MAKE-STRING-OUTPUT-STREAM"
    }

    fn description(&self) -> &str {
        "Create output stream (returns empty string)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Return empty string as output stream
        Ok(Value::String(String::new()))
    }
}

/// GET-OUTPUT-STREAM-STRING - Get string from output stream
pub struct GetOutputStreamStringTool;

impl Tool for GetOutputStreamStringTool {
    fn name(&self) -> &str {
        "GET-OUTPUT-STREAM-STRING"
    }

    fn description(&self) -> &str {
        "Get string from output stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "GET-OUTPUT-STREAM-STRING".to_string(),
                reason: "Expected stream argument".to_string(),
            });
        }

        // Stream is just a string, return it
        Ok(args[0].clone())
    }
}

// ============================================================================
// STREAM PROPERTIES (5 functions)
// ============================================================================

/// STREAM-ELEMENT-TYPE - Get element type (always :default)
pub struct StreamElementTypeTool;

impl Tool for StreamElementTypeTool {
    fn name(&self) -> &str {
        "STREAM-ELEMENT-TYPE"
    }

    fn description(&self) -> &str {
        "Get stream element type"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("CHARACTER".to_string()))
    }
}

/// INPUT-STREAM-P - Check if input stream
pub struct InputStreamPTool;

impl Tool for InputStreamPTool {
    fn name(&self) -> &str {
        "INPUT-STREAM-P"
    }

    fn description(&self) -> &str {
        "Check if value is input stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        // In OVSM, any string can be an input stream
        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

/// OUTPUT-STREAM-P - Check if output stream
pub struct OutputStreamPTool;

impl Tool for OutputStreamPTool {
    fn name(&self) -> &str {
        "OUTPUT-STREAM-P"
    }

    fn description(&self) -> &str {
        "Check if value is output stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        // In OVSM, any string can be an output stream
        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

/// INTERACTIVE-STREAM-P - Check if interactive
pub struct InteractiveStreamPTool;

impl Tool for InteractiveStreamPTool {
    fn name(&self) -> &str {
        "INTERACTIVE-STREAM-P"
    }

    fn description(&self) -> &str {
        "Check if stream is interactive"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // OVSM streams are not interactive
        Ok(Value::Bool(false))
    }
}

/// OPEN-STREAM-P - Check if stream is open
pub struct OpenStreamPTool;

impl Tool for OpenStreamPTool {
    fn name(&self) -> &str {
        "OPEN-STREAM-P"
    }

    fn description(&self) -> &str {
        "Check if stream is open"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        // In OVSM, string streams are always "open"
        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

// ============================================================================
// STREAM OPERATIONS (5 functions)
// ============================================================================

/// LISTEN - Check if input available
pub struct ListenTool;

impl Tool for ListenTool {
    fn name(&self) -> &str {
        "LISTEN"
    }

    fn description(&self) -> &str {
        "Check if input is available on stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        // Check if string is non-empty
        match &args[0] {
            Value::String(s) => Ok(Value::Bool(!s.is_empty())),
            _ => Ok(Value::Bool(false)),
        }
    }
}

/// CLEAR-INPUT - Clear input buffer
pub struct ClearInputTool;

impl Tool for ClearInputTool {
    fn name(&self) -> &str {
        "CLEAR-INPUT"
    }

    fn description(&self) -> &str {
        "Clear input buffer (returns empty string)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(String::new()))
    }
}

/// FINISH-OUTPUT - Ensure output flushed
pub struct FinishOutputTool;

impl Tool for FinishOutputTool {
    fn name(&self) -> &str {
        "FINISH-OUTPUT"
    }

    fn description(&self) -> &str {
        "Ensure output is flushed"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // In OVSM, this is a no-op, return the stream
        if args.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(args[0].clone())
        }
    }
}

/// FORCE-OUTPUT - Force output flush
pub struct ForceOutputTool;

impl Tool for ForceOutputTool {
    fn name(&self) -> &str {
        "FORCE-OUTPUT"
    }

    fn description(&self) -> &str {
        "Force output flush"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // In OVSM, this is a no-op, return the stream
        if args.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(args[0].clone())
        }
    }
}

/// CLEAR-OUTPUT - Clear output buffer
pub struct ClearOutputTool;

impl Tool for ClearOutputTool {
    fn name(&self) -> &str {
        "CLEAR-OUTPUT"
    }

    fn description(&self) -> &str {
        "Clear output buffer"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String(String::new()))
    }
}

// ============================================================================
// BINARY I/O (4 functions)
// ============================================================================

/// READ-BYTE - Read byte from stream
pub struct ReadByteTool;

impl Tool for ReadByteTool {
    fn name(&self) -> &str {
        "READ-BYTE"
    }

    fn description(&self) -> &str {
        "Read byte from stream (returns first byte)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ-BYTE".to_string(),
                reason: "Expected stream argument".to_string(),
            });
        }

        match &args[0] {
            Value::String(s) => {
                if let Some(byte) = s.bytes().next() {
                    Ok(Value::Int(byte as i64))
                } else {
                    Err(Error::InvalidArguments {
                        tool: "READ-BYTE".to_string(),
                        reason: "End of stream".to_string(),
                    })
                }
            }
            _ => Err(Error::InvalidArguments {
                tool: "READ-BYTE".to_string(),
                reason: "Expected stream (string)".to_string(),
            }),
        }
    }
}

/// WRITE-BYTE - Write byte to stream
pub struct WriteByteTool;

impl Tool for WriteByteTool {
    fn name(&self) -> &str {
        "WRITE-BYTE"
    }

    fn description(&self) -> &str {
        "Write byte to stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "WRITE-BYTE".to_string(),
                reason: "Expected byte and stream arguments".to_string(),
            });
        }

        let byte = args[0].as_int()? as u8;
        let stream = args[1].as_string()?;

        let mut result = stream.to_string();
        result.push(byte as char);

        Ok(Value::String(result))
    }
}

/// READ-SEQUENCE - Read sequence of bytes/chars
pub struct ReadSequenceTool;

impl Tool for ReadSequenceTool {
    fn name(&self) -> &str {
        "READ-SEQUENCE"
    }

    fn description(&self) -> &str {
        "Read sequence from stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "READ-SEQUENCE".to_string(),
                reason: "Expected stream argument".to_string(),
            });
        }

        // Return stream as string
        Ok(args[0].clone())
    }
}

/// WRITE-SEQUENCE - Write sequence to stream
pub struct WriteSequenceTool;

impl Tool for WriteSequenceTool {
    fn name(&self) -> &str {
        "WRITE-SEQUENCE"
    }

    fn description(&self) -> &str {
        "Write sequence to stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "WRITE-SEQUENCE".to_string(),
                reason: "Expected sequence and stream arguments".to_string(),
            });
        }

        let sequence = args[0].as_string()?;
        let stream = args[1].as_string()?;

        let result = format!("{}{}", stream, sequence);
        Ok(Value::String(result))
    }
}

// ============================================================================
// STREAM UTILITIES (8 functions)
// ============================================================================

/// WITH-OPEN-STREAM - Execute with auto-close
pub struct WithOpenStreamTool;

impl Tool for WithOpenStreamTool {
    fn name(&self) -> &str {
        "WITH-OPEN-STREAM"
    }

    fn description(&self) -> &str {
        "Execute with stream, auto-close"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "WITH-OPEN-STREAM".to_string(),
                reason: "Expected stream and body arguments".to_string(),
            });
        }
        // In OVSM, just return the stream (no actual closing needed)
        // Return result as an Arc-wrapped array for consistency
        Ok(Value::Array(Arc::new(vec![args[0].clone()])))
    }
}

/// STREAM-EXTERNAL-FORMAT - Get encoding
pub struct StreamExternalFormatTool;

impl Tool for StreamExternalFormatTool {
    fn name(&self) -> &str {
        "STREAM-EXTERNAL-FORMAT"
    }

    fn description(&self) -> &str {
        "Get stream encoding"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("UTF-8".to_string()))
    }
}

/// STREAMP - Check if value is a stream
pub struct StreampTool;

impl Tool for StreampTool {
    fn name(&self) -> &str {
        "STREAMP"
    }

    fn description(&self) -> &str {
        "Check if value is a stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        // In OVSM, strings are streams
        Ok(Value::Bool(matches!(args[0], Value::String(_))))
    }
}

/// STREAM-ERROR-STREAM - Get stream from stream-error
pub struct StreamErrorStreamTool;

impl Tool for StreamErrorStreamTool {
    fn name(&self) -> &str {
        "STREAM-ERROR-STREAM"
    }

    fn description(&self) -> &str {
        "Get stream from stream error"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Return the argument as-is
        if args.is_empty() {
            Ok(Value::Null)
        } else {
            Ok(args[0].clone())
        }
    }
}

/// PEEK-CHAR - Peek at next character without consuming
pub struct PeekCharTool;

impl Tool for PeekCharTool {
    fn name(&self) -> &str {
        "PEEK-CHAR"
    }

    fn description(&self) -> &str {
        "Peek at next character in stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "PEEK-CHAR".to_string(),
                reason: "Expected stream argument".to_string(),
            });
        }

        match &args[0] {
            Value::String(s) => {
                if let Some(ch) = s.chars().next() {
                    Ok(Value::String(ch.to_string()))
                } else {
                    Ok(Value::Null)
                }
            }
            _ => Ok(Value::Null),
        }
    }
}

/// UNREAD-CHAR - Push character back to stream
pub struct UnreadCharTool;

impl Tool for UnreadCharTool {
    fn name(&self) -> &str {
        "UNREAD-CHAR"
    }

    fn description(&self) -> &str {
        "Push character back to stream"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNREAD-CHAR".to_string(),
                reason: "Expected character and stream arguments".to_string(),
            });
        }

        let ch = args[0].as_string()?;
        let stream = args[1].as_string()?;

        let result = format!("{}{}", ch, stream);
        Ok(Value::String(result))
    }
}

/// READ-CHAR-NO-HANG - Non-blocking character read
pub struct ReadCharNoHangTool;

impl Tool for ReadCharNoHangTool {
    fn name(&self) -> &str {
        "READ-CHAR-NO-HANG"
    }

    fn description(&self) -> &str {
        "Non-blocking character read"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }

        match &args[0] {
            Value::String(s) => {
                if let Some(ch) = s.chars().next() {
                    Ok(Value::String(ch.to_string()))
                } else {
                    Ok(Value::Null)
                }
            }
            _ => Ok(Value::Null),
        }
    }
}

/// STREAM-LINE-COLUMN - Get current column number
pub struct StreamLineColumnTool;

impl Tool for StreamLineColumnTool {
    fn name(&self) -> &str {
        "STREAM-LINE-COLUMN"
    }

    fn description(&self) -> &str {
        "Get current column number in stream"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Always return 0 for simplicity
        Ok(Value::Int(0))
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all stream-related tools with the tool registry
///
/// This function registers all 25 stream operation tools including:
/// - Stream creation (3 functions)
/// - Stream properties (5 functions)
/// - Stream operations (5 functions)
/// - Binary I/O (4 functions)
/// - Stream utilities (8 functions)
pub fn register(registry: &mut ToolRegistry) {
    // Stream creation
    registry.register(MakeStringInputStreamTool);
    registry.register(MakeStringOutputStreamTool);
    registry.register(GetOutputStreamStringTool);

    // Stream properties
    registry.register(StreamElementTypeTool);
    registry.register(InputStreamPTool);
    registry.register(OutputStreamPTool);
    registry.register(InteractiveStreamPTool);
    registry.register(OpenStreamPTool);

    // Stream operations
    registry.register(ListenTool);
    registry.register(ClearInputTool);
    registry.register(FinishOutputTool);
    registry.register(ForceOutputTool);
    registry.register(ClearOutputTool);

    // Binary I/O
    registry.register(ReadByteTool);
    registry.register(WriteByteTool);
    registry.register(ReadSequenceTool);
    registry.register(WriteSequenceTool);

    // Stream utilities
    registry.register(WithOpenStreamTool);
    registry.register(StreamExternalFormatTool);
    registry.register(StreampTool);
    registry.register(StreamErrorStreamTool);
    registry.register(PeekCharTool);
    registry.register(UnreadCharTool);
    registry.register(ReadCharNoHangTool);
    registry.register(StreamLineColumnTool);
}
