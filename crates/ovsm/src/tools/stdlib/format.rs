//! Format operations for OVSM
//!
//! This module implements Common Lisp's FORMAT function with directives:
//! - ~A - ASCII output (aesthetic)
//! - ~S - S-expression output (readable)
//! - ~D - Decimal integer
//! - ~X - Hexadecimal
//! - ~O - Octal
//! - ~B - Binary
//! - ~F - Fixed-format floating point
//! - ~E - Exponential floating point
//! - ~% - Newline
//! - ~& - Fresh line
//! - ~~ - Tilde literal
//! - ~T - Tabulate (spaces)
//! - ~* - Skip argument
//! - ~C - Character output
//!
//! Implementation notes:
//! - Simplified compared to full Common Lisp FORMAT
//! - Supports basic directives without complex modifiers
//! - Format string is parsed character by character

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

// ============================================================================
// FORMAT MAIN FUNCTION
// ============================================================================

/// FORMAT - Main format function with directive support
pub struct FormatTool;

impl Tool for FormatTool {
    fn name(&self) -> &str {
        "FORMAT"
    }

    fn description(&self) -> &str {
        "Format string with directives (~A, ~D, ~X, etc.)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT".to_string(),
                reason: "Expected format string and arguments".to_string(),
            });
        }

        // First argument is NIL (output to stdout) or T, or a string
        // For simplicity, we'll always return a string
        let format_string = if args.len() == 1 {
            args[0].as_string()?
        } else {
            args[1].as_string()?
        };

        let format_args = if args.len() > 1 { &args[1..] } else { &[] };

        let result = parse_format(format_string, format_args)?;
        Ok(Value::String(result))
    }
}

/// Parse format string and apply directives
fn parse_format(format_str: &str, args: &[Value]) -> Result<String> {
    let mut result = String::new();
    let mut chars = format_str.chars().peekable();
    let mut arg_index = 0;

    while let Some(ch) = chars.next() {
        if ch == '~' {
            // Process directive
            if let Some(directive) = chars.next() {
                match directive.to_ascii_uppercase() {
                    'A' => {
                        // ~A - ASCII output
                        if arg_index < args.len() {
                            result.push_str(&format_a(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'S' => {
                        // ~S - S-expression output
                        if arg_index < args.len() {
                            result.push_str(&format_s(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'D' => {
                        // ~D - Decimal integer
                        if arg_index < args.len() {
                            result.push_str(&format_d(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'X' => {
                        // ~X - Hexadecimal
                        if arg_index < args.len() {
                            result.push_str(&format_x(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'O' => {
                        // ~O - Octal
                        if arg_index < args.len() {
                            result.push_str(&format_o(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'B' => {
                        // ~B - Binary
                        if arg_index < args.len() {
                            result.push_str(&format_b(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'F' => {
                        // ~F - Fixed-format floating
                        if arg_index < args.len() {
                            result.push_str(&format_f(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    'E' => {
                        // ~E - Exponential floating
                        if arg_index < args.len() {
                            result.push_str(&format_e(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    '%' => {
                        // ~% - Newline
                        result.push('\n');
                    }
                    '&' => {
                        // ~& - Fresh line (simplified: always newline)
                        result.push('\n');
                    }
                    '~' => {
                        // ~~ - Tilde literal
                        result.push('~');
                    }
                    'T' => {
                        // ~T - Tabulate (insert spaces)
                        result.push_str("    ");
                    }
                    '*' => {
                        // ~* - Skip argument
                        arg_index += 1;
                    }
                    'C' => {
                        // ~C - Character output
                        if arg_index < args.len() {
                            result.push_str(&format_c(&args[arg_index])?);
                            arg_index += 1;
                        }
                    }
                    _ => {
                        // Unknown directive, output as-is
                        result.push('~');
                        result.push(directive);
                    }
                }
            }
        } else {
            result.push(ch);
        }
    }

    Ok(result)
}

// ============================================================================
// FORMAT DIRECTIVE IMPLEMENTATIONS
// ============================================================================

/// ~A - ASCII/Aesthetic output (no quotes on strings)
fn format_a(val: &Value) -> Result<String> {
    Ok(match val {
        Value::String(s) => s.clone(),
        _ => val.to_string(),
    })
}

/// ~S - S-expression output (with quotes on strings)
fn format_s(val: &Value) -> Result<String> {
    Ok(match val {
        Value::String(s) => format!("\"{}\"", s),
        _ => val.to_string(),
    })
}

/// ~D - Decimal integer output
fn format_d(val: &Value) -> Result<String> {
    match val {
        Value::Int(n) => Ok(n.to_string()),
        Value::Float(f) => Ok((*f as i64).to_string()),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~D requires numeric argument".to_string(),
        }),
    }
}

/// ~X - Hexadecimal output
fn format_x(val: &Value) -> Result<String> {
    match val {
        Value::Int(n) => Ok(format!("{:x}", n)),
        Value::Float(f) => Ok(format!("{:x}", *f as i64)),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~X requires numeric argument".to_string(),
        }),
    }
}

/// ~O - Octal output
fn format_o(val: &Value) -> Result<String> {
    match val {
        Value::Int(n) => Ok(format!("{:o}", n)),
        Value::Float(f) => Ok(format!("{:o}", *f as i64)),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~O requires numeric argument".to_string(),
        }),
    }
}

/// ~B - Binary output
fn format_b(val: &Value) -> Result<String> {
    match val {
        Value::Int(n) => Ok(format!("{:b}", n)),
        Value::Float(f) => Ok(format!("{:b}", *f as i64)),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~B requires numeric argument".to_string(),
        }),
    }
}

/// ~F - Fixed-format floating point
fn format_f(val: &Value) -> Result<String> {
    match val {
        Value::Float(f) => Ok(format!("{:.2}", f)),
        Value::Int(n) => Ok(format!("{:.2}", *n as f64)),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~F requires numeric argument".to_string(),
        }),
    }
}

/// ~E - Exponential floating point
fn format_e(val: &Value) -> Result<String> {
    match val {
        Value::Float(f) => Ok(format!("{:e}", f)),
        Value::Int(n) => Ok(format!("{:e}", *n as f64)),
        _ => Err(Error::InvalidArguments {
            tool: "FORMAT".to_string(),
            reason: "~E requires numeric argument".to_string(),
        }),
    }
}

/// ~C - Character output
fn format_c(val: &Value) -> Result<String> {
    match val {
        Value::String(s) => {
            if s.len() == 1 {
                Ok(s.clone())
            } else {
                Ok(s.chars().next().unwrap_or(' ').to_string())
            }
        }
        Value::Int(n) => {
            // Treat as character code
            if let Some(ch) = char::from_u32(*n as u32) {
                Ok(ch.to_string())
            } else {
                Ok("?".to_string())
            }
        }
        _ => Ok(val.to_string()),
    }
}

// ============================================================================
// INDIVIDUAL DIRECTIVE TOOLS (for direct access)
// ============================================================================

/// FORMAT-A - ASCII output directive
pub struct FormatATool;

impl Tool for FormatATool {
    fn name(&self) -> &str {
        "FORMAT-A"
    }

    fn description(&self) -> &str {
        "Format value as ASCII (aesthetic) output"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-A".to_string(),
                reason: "Expected value to format".to_string(),
            });
        }

        Ok(Value::String(format_a(&args[0])?))
    }
}

/// FORMAT-S - S-expression output directive
pub struct FormatSTool;

impl Tool for FormatSTool {
    fn name(&self) -> &str {
        "FORMAT-S"
    }

    fn description(&self) -> &str {
        "Format value as S-expression (readable) output"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-S".to_string(),
                reason: "Expected value to format".to_string(),
            });
        }

        Ok(Value::String(format_s(&args[0])?))
    }
}

/// FORMAT-D - Decimal output directive
pub struct FormatDTool;

impl Tool for FormatDTool {
    fn name(&self) -> &str {
        "FORMAT-D"
    }

    fn description(&self) -> &str {
        "Format number as decimal integer"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-D".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_d(&args[0])?))
    }
}

/// FORMAT-X - Hexadecimal output directive
pub struct FormatXTool;

impl Tool for FormatXTool {
    fn name(&self) -> &str {
        "FORMAT-X"
    }

    fn description(&self) -> &str {
        "Format number as hexadecimal"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-X".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_x(&args[0])?))
    }
}

/// FORMAT-O - Octal output directive
pub struct FormatOTool;

impl Tool for FormatOTool {
    fn name(&self) -> &str {
        "FORMAT-O"
    }

    fn description(&self) -> &str {
        "Format number as octal"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-O".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_o(&args[0])?))
    }
}

/// FORMAT-B - Binary output directive
pub struct FormatBTool;

impl Tool for FormatBTool {
    fn name(&self) -> &str {
        "FORMAT-B"
    }

    fn description(&self) -> &str {
        "Format number as binary"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-B".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_b(&args[0])?))
    }
}

/// FORMAT-F - Fixed floating point directive
pub struct FormatFTool;

impl Tool for FormatFTool {
    fn name(&self) -> &str {
        "FORMAT-F"
    }

    fn description(&self) -> &str {
        "Format number as fixed-point floating"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-F".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_f(&args[0])?))
    }
}

/// FORMAT-E - Exponential floating point directive
pub struct FormatETool;

impl Tool for FormatETool {
    fn name(&self) -> &str {
        "FORMAT-E"
    }

    fn description(&self) -> &str {
        "Format number as exponential floating"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-E".to_string(),
                reason: "Expected numeric value".to_string(),
            });
        }

        Ok(Value::String(format_e(&args[0])?))
    }
}

/// FORMAT-C - Character output directive
pub struct FormatCTool;

impl Tool for FormatCTool {
    fn name(&self) -> &str {
        "FORMAT-C"
    }

    fn description(&self) -> &str {
        "Format value as character"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "FORMAT-C".to_string(),
                reason: "Expected value to format".to_string(),
            });
        }

        Ok(Value::String(format_c(&args[0])?))
    }
}

/// FORMAT-NEWLINE - Newline directive (~%)
pub struct FormatNewlineTool;

impl Tool for FormatNewlineTool {
    fn name(&self) -> &str {
        "FORMAT-NEWLINE"
    }

    fn description(&self) -> &str {
        "Output newline character"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("\n".to_string()))
    }
}

/// FORMAT-TILDE - Tilde literal directive (~~)
pub struct FormatTildeTool;

impl Tool for FormatTildeTool {
    fn name(&self) -> &str {
        "FORMAT-TILDE"
    }

    fn description(&self) -> &str {
        "Output tilde literal"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("~".to_string()))
    }
}

/// FORMAT-TAB - Tabulate directive (~T)
pub struct FormatTabTool;

impl Tool for FormatTabTool {
    fn name(&self) -> &str {
        "FORMAT-TAB"
    }

    fn description(&self) -> &str {
        "Output tab/spaces"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("    ".to_string()))
    }
}

/// FORMAT-FRESH-LINE - Fresh line directive (~&)
pub struct FormatFreshLineTool;

impl Tool for FormatFreshLineTool {
    fn name(&self) -> &str {
        "FORMAT-FRESH-LINE"
    }

    fn description(&self) -> &str {
        "Output newline if needed"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("\n".to_string()))
    }
}

// ============================================================================
// REGISTRATION
// ============================================================================

/// Register all format tools
pub fn register(registry: &mut ToolRegistry) {
    // Main FORMAT function
    registry.register(FormatTool);

    // Individual directive tools
    registry.register(FormatATool);
    registry.register(FormatSTool);
    registry.register(FormatDTool);
    registry.register(FormatXTool);
    registry.register(FormatOTool);
    registry.register(FormatBTool);
    registry.register(FormatFTool);
    registry.register(FormatETool);
    registry.register(FormatCTool);
    registry.register(FormatNewlineTool);
    registry.register(FormatTildeTool);
    registry.register(FormatTabTool);
    registry.register(FormatFreshLineTool);
    registry.register(FormatSkipTool);
}

/// FORMAT-SKIP - Skip argument directive (~*)
pub struct FormatSkipTool;

impl Tool for FormatSkipTool {
    fn name(&self) -> &str {
        "FORMAT-SKIP"
    }

    fn description(&self) -> &str {
        "Skip current argument in format sequence"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        // Returns empty string, argument is skipped
        Ok(Value::String(String::new()))
    }
}
