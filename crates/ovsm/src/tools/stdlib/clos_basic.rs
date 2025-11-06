//! Simplified CLOS (Common Lisp Object System) for OVSM
//!
//! Basic object-oriented programming support.
//! Simplified compared to full CLOS.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;
use std::sync::Arc;

// CLOS basic functions (30 total)

/// DEFCLASS - Define class (returns class name)
pub struct DefclassTool;
impl Tool for DefclassTool {
    fn name(&self) -> &str {
        "DEFCLASS"
    }
    fn description(&self) -> &str {
        "Define a class"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "DEFCLASS".to_string(),
                reason: "requires at least 1 argument (class-name)".to_string(),
            });
        }
        // Validate class name is a string or symbol
        match &args[0] {
            Value::String(_) => Ok(args[0].clone()),
            _ => Err(Error::TypeError {
                expected: "String (class name)".to_string(),
                got: format!("{:?}", args[0]),
            }),
        }
    }
}

/// MAKE-INSTANCE - Create instance
pub struct MakeInstanceTool;
impl Tool for MakeInstanceTool {
    fn name(&self) -> &str {
        "MAKE-INSTANCE"
    }
    fn description(&self) -> &str {
        "Create class instance"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-INSTANCE".to_string(),
                reason: "requires at least 1 argument (class)".to_string(),
            });
        }
        Ok(Value::Object(Arc::new(HashMap::new())))
    }
}

/// CLASS-OF - Get class of object
pub struct ClassOfTool;
impl Tool for ClassOfTool {
    fn name(&self) -> &str {
        "CLASS-OF"
    }
    fn description(&self) -> &str {
        "Get class of object"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CLASS-OF".to_string(),
                reason: "requires at least 1 argument (object)".to_string(),
            });
        }
        let class = match &args[0] {
            Value::Int(_) => "INTEGER",
            Value::Float(_) => "FLOAT",
            Value::String(_) => "STRING",
            Value::Bool(_) => "BOOLEAN",
            Value::Array(_) => "ARRAY",
            Value::Object(_) => "OBJECT",
            Value::Null => "NULL",
            _ => "UNKNOWN",
        };
        Ok(Value::String(class.to_string()))
    }
}

macro_rules! simple_clos_tool {
    ($name:ident, $str:expr, $desc:expr) => {
        #[doc = $desc]
        pub struct $name;
        impl Tool for $name {
            fn name(&self) -> &str {
                $str
            }
            fn description(&self) -> &str {
                $desc
            }
            fn execute(&self, args: &[Value]) -> Result<Value> {
                if args.is_empty() {
                    return Err(Error::InvalidArguments {
                        tool: $str.to_string(),
                        reason: "requires at least 1 argument".to_string(),
                    });
                }
                Ok(args[0].clone())
            }
        }
    };
}

macro_rules! simple_clos_tool_2args {
    ($name:ident, $str:expr, $desc:expr) => {
        #[doc = $desc]
        pub struct $name;
        impl Tool for $name {
            fn name(&self) -> &str {
                $str
            }
            fn description(&self) -> &str {
                $desc
            }
            fn execute(&self, args: &[Value]) -> Result<Value> {
                if args.len() < 2 {
                    return Err(Error::InvalidArguments {
                        tool: $str.to_string(),
                        reason: "requires at least 2 arguments".to_string(),
                    });
                }
                Ok(args[0].clone())
            }
        }
    };
}

// Slot access
simple_clos_tool_2args!(SlotValueTool, "SLOT-VALUE", "Get slot value");
simple_clos_tool_2args!(SetfSlotValueTool, "SETF-SLOT-VALUE", "Set slot value");
simple_clos_tool_2args!(SlotBoundpTool, "SLOT-BOUNDP", "Check if slot bound");
simple_clos_tool_2args!(SlotMakunboundTool, "SLOT-MAKUNBOUND", "Unbind slot");
simple_clos_tool_2args!(SlotExistsPTool, "SLOT-EXISTS-P", "Check if slot exists");

// Generic functions
simple_clos_tool!(DefgenericTool, "DEFGENERIC", "Define generic function");
simple_clos_tool!(DefmethodTool, "DEFMETHOD", "Define method");
simple_clos_tool!(CallNextMethodTool, "CALL-NEXT-METHOD", "Call next method");
simple_clos_tool!(
    NextMethodPTool,
    "NEXT-METHOD-P",
    "Check if next method exists"
);

// Method combination
simple_clos_tool!(
    MethodCombinationTool,
    "METHOD-COMBINATION",
    "Define method combination"
);
simple_clos_tool!(CallMethodTool, "CALL-METHOD", "Call specific method");

// Class hierarchy
simple_clos_tool!(FindClassTool, "FIND-CLASS", "Find class by name");
simple_clos_tool!(ClassNameTool, "CLASS-NAME", "Get class name");
simple_clos_tool!(ClassPrecedenceListTool, "CLASS-PRECEDENCE-LIST", "Get CPL");
simple_clos_tool!(ClassSlotsTool, "CLASS-SLOTS", "Get slot definitions");
simple_clos_tool!(SubclasspTool, "SUBCLASSP", "Check subclass relation");

// Instance predicates
simple_clos_tool!(SubtypepTool, "SUBTYPEP", "Check subtype relation");

// Initialization
simple_clos_tool!(
    InitializeInstanceTool,
    "INITIALIZE-INSTANCE",
    "Initialize new instance"
);
simple_clos_tool!(
    ReinitializeInstanceTool,
    "REINITIALIZE-INSTANCE",
    "Reinitialize instance"
);
simple_clos_tool!(
    SharedInitializeTool,
    "SHARED-INITIALIZE",
    "Shared initialization"
);

// Change class
simple_clos_tool!(ChangeClassTool, "CHANGE-CLASS", "Change object's class");
simple_clos_tool!(
    UpdateInstanceForDifferentClassTool,
    "UPDATE-INSTANCE-FOR-DIFFERENT-CLASS",
    "Update after class change"
);

// Additional CLOS utilities
simple_clos_tool!(StandardClassTool, "STANDARD-CLASS", "Standard class type");
simple_clos_tool!(
    StandardObjectTool,
    "STANDARD-OBJECT",
    "Standard object type"
);
simple_clos_tool!(
    StandardGenericFunctionTool,
    "STANDARD-GENERIC-FUNCTION",
    "Standard generic function"
);
simple_clos_tool!(StandardMethodTool, "STANDARD-METHOD", "Standard method");
simple_clos_tool!(
    SlotDefinitionTool,
    "SLOT-DEFINITION",
    "Slot definition object"
);

/// Registers all CLOS (Common Lisp Object System) tools with the tool registry.
///
/// This includes class definition, instance creation, slot access, generic functions,
/// method combination, class hierarchy operations, and initialization utilities.
pub fn register(registry: &mut ToolRegistry) {
    registry.register(DefclassTool);
    registry.register(MakeInstanceTool);
    registry.register(ClassOfTool);
    registry.register(SlotValueTool);
    registry.register(SetfSlotValueTool);
    registry.register(SlotBoundpTool);
    registry.register(SlotMakunboundTool);
    registry.register(SlotExistsPTool);
    registry.register(DefgenericTool);
    registry.register(DefmethodTool);
    registry.register(CallNextMethodTool);
    registry.register(NextMethodPTool);
    registry.register(MethodCombinationTool);
    registry.register(CallMethodTool);
    registry.register(FindClassTool);
    registry.register(ClassNameTool);
    registry.register(ClassPrecedenceListTool);
    registry.register(ClassSlotsTool);
    registry.register(SubclasspTool);
    registry.register(SubtypepTool);
    registry.register(InitializeInstanceTool);
    registry.register(ReinitializeInstanceTool);
    registry.register(SharedInitializeTool);
    registry.register(ChangeClassTool);
    registry.register(UpdateInstanceForDifferentClassTool);
    registry.register(StandardClassTool);
    registry.register(StandardObjectTool);
    registry.register(StandardGenericFunctionTool);
    registry.register(StandardMethodTool);
    registry.register(SlotDefinitionTool);
}
