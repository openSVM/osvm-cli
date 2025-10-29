//! Simplified condition system for OVSM
//!
//! Provides basic error handling and condition signaling.
//! Simplified compared to full Common Lisp condition system.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};

// Condition type functions (35 total)

/// ERROR - Signal error
pub struct ErrorTool;
impl Tool for ErrorTool {
    fn name(&self) -> &str { "ERROR" }
    fn description(&self) -> &str { "Signal an error condition" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let msg = if args.is_empty() { "Error" } else { args[0].as_string()? };
        Err(Error::ToolExecutionError {
            tool: "ERROR".to_string(),
            reason: msg.to_string(),
        })
    }
}

/// CERROR - Continuable error
pub struct CerrorTool;
impl Tool for CerrorTool {
    fn name(&self) -> &str { "CERROR" }
    fn description(&self) -> &str { "Signal continuable error" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let msg = if args.is_empty() { "Continuable error" } else { args[0].as_string()? };
        Ok(Value::String(format!("CERROR: {}", msg)))
    }
}

/// WARN - Signal warning
pub struct WarnTool;
impl Tool for WarnTool {
    fn name(&self) -> &str { "WARN" }
    fn description(&self) -> &str { "Signal warning" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let msg = if args.is_empty() { "Warning" } else { args[0].as_string()? };
        eprintln!("WARNING: {}", msg);
        Ok(Value::Null)
    }
}

/// SIGNAL - Signal condition
pub struct SignalTool;
impl Tool for SignalTool {
    fn name(&self) -> &str { "SIGNAL" }
    fn description(&self) -> &str { "Signal condition" }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if !args.is_empty() {
            eprintln!("SIGNAL: {}", args[0]);
        }
        Ok(Value::Null)
    }
}

// Simple macro-like tools (these would be macros in full CL)
macro_rules! simple_condition_tool {
    ($name:ident, $str:expr, $desc:expr) => {
        pub struct $name;
        impl Tool for $name {
            fn name(&self) -> &str { $str }
            fn description(&self) -> &str { $desc }
            fn execute(&self, args: &[Value]) -> Result<Value> {
                Ok(if args.is_empty() { Value::Null } else { args[0].clone() })
            }
        }
    };
}

simple_condition_tool!(HandlerBindTool, "HANDLER-BIND", "Bind condition handlers");
simple_condition_tool!(HandlerCaseTool, "HANDLER-CASE", "Handle conditions with cases");
simple_condition_tool!(IgnoreErrorsTool, "IGNORE-ERRORS", "Suppress errors");
simple_condition_tool!(WithSimpleRestartTool, "WITH-SIMPLE-RESTART", "Provide simple restart");
simple_condition_tool!(RestartCaseTool, "RESTART-CASE", "Define restarts");
simple_condition_tool!(RestartBindTool, "RESTART-BIND", "Bind restarts");
simple_condition_tool!(InvokeRestartTool, "INVOKE-RESTART", "Invoke named restart");
simple_condition_tool!(FindRestartTool, "FIND-RESTART", "Find restart by name");
simple_condition_tool!(ComputeRestartsTool, "COMPUTE-RESTARTS", "List available restarts");
simple_condition_tool!(MakeConditionTool, "MAKE-CONDITION", "Create condition object");
simple_condition_tool!(ConditionTypeTool, "CONDITION-TYPE", "Get condition type");
simple_condition_tool!(SimpleConditionFormatControlTool, "SIMPLE-CONDITION-FORMAT-CONTROL", "Get format string");
simple_condition_tool!(SimpleConditionFormatArgumentsTool, "SIMPLE-CONDITION-FORMAT-ARGUMENTS", "Get format args");

// Standard condition types
simple_condition_tool!(SimpleErrorTool, "SIMPLE-ERROR", "Basic error type");
simple_condition_tool!(SimpleWarningTool, "SIMPLE-WARNING", "Basic warning type");
simple_condition_tool!(TypeErrorTool, "TYPE-ERROR", "Type mismatch error");
simple_condition_tool!(ProgramErrorTool, "PROGRAM-ERROR", "Program error");
simple_condition_tool!(ControlErrorTool, "CONTROL-ERROR", "Control flow error");
simple_condition_tool!(StreamErrorTool, "STREAM-ERROR", "Stream operation error");
simple_condition_tool!(FileErrorTool, "FILE-ERROR", "File operation error");
simple_condition_tool!(ArithmeticErrorTool, "ARITHMETIC-ERROR", "Math error");
simple_condition_tool!(DivisionByZeroTool, "DIVISION-BY-ZERO", "Division by zero");
simple_condition_tool!(FloatingPointOverflowTool, "FLOATING-POINT-OVERFLOW", "Float overflow");
simple_condition_tool!(FloatingPointUnderflowTool, "FLOATING-POINT-UNDERFLOW", "Float underflow");

// Condition predicates
simple_condition_tool!(ConditionPTool, "CONDITION-P", "Check if condition");
simple_condition_tool!(ErrorPTool, "ERROR-P", "Check if error");
simple_condition_tool!(WarningPTool, "WARNING-P", "Check if warning");

// Restart utilities
simple_condition_tool!(RestartNameTool, "RESTART-NAME", "Get restart name");
simple_condition_tool!(InvokeRestartInteractivelyTool, "INVOKE-RESTART-INTERACTIVELY", "Invoke restart interactively");
simple_condition_tool!(AbortTool, "ABORT", "Abort to toplevel");
simple_condition_tool!(ContinueTool, "CONTINUE", "Continue from error");
simple_condition_tool!(StorValueTool, "STORE-VALUE", "Store value restart");
simple_condition_tool!(UseValueTool, "USE-VALUE", "Use value restart");

pub fn register(registry: &mut ToolRegistry) {
    registry.register(ErrorTool);
    registry.register(CerrorTool);
    registry.register(WarnTool);
    registry.register(SignalTool);
    registry.register(HandlerBindTool);
    registry.register(HandlerCaseTool);
    registry.register(IgnoreErrorsTool);
    registry.register(WithSimpleRestartTool);
    registry.register(RestartCaseTool);
    registry.register(RestartBindTool);
    registry.register(InvokeRestartTool);
    registry.register(FindRestartTool);
    registry.register(ComputeRestartsTool);
    registry.register(MakeConditionTool);
    registry.register(ConditionTypeTool);
    registry.register(SimpleConditionFormatControlTool);
    registry.register(SimpleConditionFormatArgumentsTool);
    registry.register(SimpleErrorTool);
    registry.register(SimpleWarningTool);
    registry.register(TypeErrorTool);
    registry.register(ProgramErrorTool);
    registry.register(ControlErrorTool);
    registry.register(StreamErrorTool);
    registry.register(FileErrorTool);
    registry.register(ArithmeticErrorTool);
    registry.register(DivisionByZeroTool);
    registry.register(FloatingPointOverflowTool);
    registry.register(FloatingPointUnderflowTool);
    registry.register(ConditionPTool);
    registry.register(ErrorPTool);
    registry.register(WarningPTool);
    registry.register(RestartNameTool);
    registry.register(InvokeRestartInteractivelyTool);
    registry.register(AbortTool);
    registry.register(ContinueTool);
    registry.register(StorValueTool);
    registry.register(UseValueTool);
}
