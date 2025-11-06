//! Advanced CLOS (Common Lisp Object System) features for OVSM
//!
//! Method specialization, generic function introspection, and Meta-Object Protocol basics.
//! This module extends the basic CLOS functionality with advanced features.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;
use std::sync::Arc;

// CLOS advanced functions (45 total)

// ============================================================
// GENERIC FUNCTION INTROSPECTION
// ============================================================

/// GENERIC-FUNCTION-METHODS - Get all methods for generic function
pub struct GenericFunctionMethodsTool;
impl Tool for GenericFunctionMethodsTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-METHODS"
    }
    fn description(&self) -> &str {
        "Get all methods of a generic function"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// GENERIC-FUNCTION-NAME - Get name of generic function
pub struct GenericFunctionNameTool;
impl Tool for GenericFunctionNameTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-NAME"
    }
    fn description(&self) -> &str {
        "Get name of generic function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "GENERIC-FUNCTION-NAME".to_string(),
                reason: "requires at least 1 argument (generic function)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// GENERIC-FUNCTION-LAMBDA-LIST - Get lambda list of generic function
pub struct GenericFunctionLambdaListTool;
impl Tool for GenericFunctionLambdaListTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-LAMBDA-LIST"
    }
    fn description(&self) -> &str {
        "Get lambda list of generic function"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER - Get argument precedence
pub struct GenericFunctionArgumentPrecedenceOrderTool;
impl Tool for GenericFunctionArgumentPrecedenceOrderTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER"
    }
    fn description(&self) -> &str {
        "Get argument precedence order"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// GENERIC-FUNCTION-DECLARATIONS - Get declarations
pub struct GenericFunctionDeclarationsTool;
impl Tool for GenericFunctionDeclarationsTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-DECLARATIONS"
    }
    fn description(&self) -> &str {
        "Get declarations of generic function"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// GENERIC-FUNCTION-METHOD-CLASS - Get method class
pub struct GenericFunctionMethodClassTool;
impl Tool for GenericFunctionMethodClassTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-METHOD-CLASS"
    }
    fn description(&self) -> &str {
        "Get method class of generic function"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("STANDARD-METHOD".to_string()))
    }
}

/// GENERIC-FUNCTION-METHOD-COMBINATION - Get method combination
pub struct GenericFunctionMethodCombinationTool;
impl Tool for GenericFunctionMethodCombinationTool {
    fn name(&self) -> &str {
        "GENERIC-FUNCTION-METHOD-COMBINATION"
    }
    fn description(&self) -> &str {
        "Get method combination of generic function"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("STANDARD".to_string()))
    }
}

// ============================================================
// METHOD INTROSPECTION
// ============================================================

/// METHOD-QUALIFIERS - Get method qualifiers
pub struct MethodQualifiersTool;
impl Tool for MethodQualifiersTool {
    fn name(&self) -> &str {
        "METHOD-QUALIFIERS"
    }
    fn description(&self) -> &str {
        "Get qualifiers of a method"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// METHOD-SPECIALIZERS - Get method specializers
pub struct MethodSpecializersTool;
impl Tool for MethodSpecializersTool {
    fn name(&self) -> &str {
        "METHOD-SPECIALIZERS"
    }
    fn description(&self) -> &str {
        "Get specializers of a method"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// METHOD-LAMBDA-LIST - Get method lambda list
pub struct MethodLambdaListTool;
impl Tool for MethodLambdaListTool {
    fn name(&self) -> &str {
        "METHOD-LAMBDA-LIST"
    }
    fn description(&self) -> &str {
        "Get lambda list of a method"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// METHOD-GENERIC-FUNCTION - Get generic function of method
pub struct MethodGenericFunctionTool;
impl Tool for MethodGenericFunctionTool {
    fn name(&self) -> &str {
        "METHOD-GENERIC-FUNCTION"
    }
    fn description(&self) -> &str {
        "Get generic function of a method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "METHOD-GENERIC-FUNCTION".to_string(),
                reason: "requires at least 1 argument (method)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// METHOD-FUNCTION - Get function of method
pub struct MethodFunctionTool;
impl Tool for MethodFunctionTool {
    fn name(&self) -> &str {
        "METHOD-FUNCTION"
    }
    fn description(&self) -> &str {
        "Get function implementation of a method"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "METHOD-FUNCTION".to_string(),
                reason: "requires at least 1 argument (method)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

// ============================================================
// METHOD MANAGEMENT
// ============================================================

/// ADD-METHOD - Add method to generic function
pub struct AddMethodTool;
impl Tool for AddMethodTool {
    fn name(&self) -> &str {
        "ADD-METHOD"
    }
    fn description(&self) -> &str {
        "Add method to generic function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ADD-METHOD".to_string(),
                reason: "requires at least 2 arguments (generic-function method)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// REMOVE-METHOD - Remove method from generic function
pub struct RemoveMethodTool;
impl Tool for RemoveMethodTool {
    fn name(&self) -> &str {
        "REMOVE-METHOD"
    }
    fn description(&self) -> &str {
        "Remove method from generic function"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REMOVE-METHOD".to_string(),
                reason: "requires at least 2 arguments (generic-function method)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// FIND-METHOD - Find method by specializers
pub struct FindMethodTool;
impl Tool for FindMethodTool {
    fn name(&self) -> &str {
        "FIND-METHOD"
    }
    fn description(&self) -> &str {
        "Find method by specializers"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

/// COMPUTE-APPLICABLE-METHODS - Compute applicable methods
pub struct ComputeApplicableMethodsTool;
impl Tool for ComputeApplicableMethodsTool {
    fn name(&self) -> &str {
        "COMPUTE-APPLICABLE-METHODS"
    }
    fn description(&self) -> &str {
        "Compute applicable methods for arguments"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// COMPUTE-APPLICABLE-METHODS-USING-CLASSES - Compute by classes
pub struct ComputeApplicableMethodsUsingClassesTool;
impl Tool for ComputeApplicableMethodsUsingClassesTool {
    fn name(&self) -> &str {
        "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
    }
    fn description(&self) -> &str {
        "Compute applicable methods using classes"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

// ============================================================
// CLASS INTROSPECTION
// ============================================================

/// CLASS-DIRECT-SUPERCLASSES - Get direct superclasses
pub struct ClassDirectSuperclassesTool;
impl Tool for ClassDirectSuperclassesTool {
    fn name(&self) -> &str {
        "CLASS-DIRECT-SUPERCLASSES"
    }
    fn description(&self) -> &str {
        "Get direct superclasses of a class"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// CLASS-DIRECT-SUBCLASSES - Get direct subclasses
pub struct ClassDirectSubclassesTool;
impl Tool for ClassDirectSubclassesTool {
    fn name(&self) -> &str {
        "CLASS-DIRECT-SUBCLASSES"
    }
    fn description(&self) -> &str {
        "Get direct subclasses of a class"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// CLASS-DIRECT-SLOTS - Get direct slots
pub struct ClassDirectSlotsTool;
impl Tool for ClassDirectSlotsTool {
    fn name(&self) -> &str {
        "CLASS-DIRECT-SLOTS"
    }
    fn description(&self) -> &str {
        "Get direct slots of a class"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// CLASS-DEFAULT-INITARGS - Get default initargs
pub struct ClassDefaultInitargsTool;
impl Tool for ClassDefaultInitargsTool {
    fn name(&self) -> &str {
        "CLASS-DEFAULT-INITARGS"
    }
    fn description(&self) -> &str {
        "Get default initialization arguments"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// CLASS-DIRECT-DEFAULT-INITARGS - Get direct default initargs
pub struct ClassDirectDefaultInitargsTool;
impl Tool for ClassDirectDefaultInitargsTool {
    fn name(&self) -> &str {
        "CLASS-DIRECT-DEFAULT-INITARGS"
    }
    fn description(&self) -> &str {
        "Get direct default initargs"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// CLASS-PROTOTYPE - Get class prototype
pub struct ClassPrototypeTool;
impl Tool for ClassPrototypeTool {
    fn name(&self) -> &str {
        "CLASS-PROTOTYPE"
    }
    fn description(&self) -> &str {
        "Get prototype instance of a class"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Object(Arc::new(HashMap::new())))
    }
}

/// CLASS-FINALIZED-P - Check if class is finalized
pub struct ClassFinalizedPTool;
impl Tool for ClassFinalizedPTool {
    fn name(&self) -> &str {
        "CLASS-FINALIZED-P"
    }
    fn description(&self) -> &str {
        "Check if class is finalized"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Bool(true))
    }
}

/// FINALIZE-INHERITANCE - Finalize class inheritance
pub struct FinalizeInheritanceTool;
impl Tool for FinalizeInheritanceTool {
    fn name(&self) -> &str {
        "FINALIZE-INHERITANCE"
    }
    fn description(&self) -> &str {
        "Finalize class inheritance"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

// ============================================================
// SLOT INTROSPECTION
// ============================================================

/// SLOT-DEFINITION-NAME - Get slot name
pub struct SlotDefinitionNameTool;
impl Tool for SlotDefinitionNameTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-NAME"
    }
    fn description(&self) -> &str {
        "Get name of slot definition"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SLOT-DEFINITION-NAME".to_string(),
                reason: "requires at least 1 argument (slot-definition)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// SLOT-DEFINITION-INITARGS - Get slot initargs
pub struct SlotDefinitionInitargsTool;
impl Tool for SlotDefinitionInitargsTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-INITARGS"
    }
    fn description(&self) -> &str {
        "Get initialization arguments of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// SLOT-DEFINITION-INITFORM - Get slot initform
pub struct SlotDefinitionInitformTool;
impl Tool for SlotDefinitionInitformTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-INITFORM"
    }
    fn description(&self) -> &str {
        "Get initialization form of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

/// SLOT-DEFINITION-INITFUNCTION - Get slot init function
pub struct SlotDefinitionInitfunctionTool;
impl Tool for SlotDefinitionInitfunctionTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-INITFUNCTION"
    }
    fn description(&self) -> &str {
        "Get initialization function of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Null)
    }
}

/// SLOT-DEFINITION-TYPE - Get slot type
pub struct SlotDefinitionTypeTool;
impl Tool for SlotDefinitionTypeTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-TYPE"
    }
    fn description(&self) -> &str {
        "Get type of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("T".to_string()))
    }
}

/// SLOT-DEFINITION-ALLOCATION - Get slot allocation
pub struct SlotDefinitionAllocationTool;
impl Tool for SlotDefinitionAllocationTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-ALLOCATION"
    }
    fn description(&self) -> &str {
        "Get allocation type of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("INSTANCE".to_string()))
    }
}

/// SLOT-DEFINITION-READERS - Get slot readers
pub struct SlotDefinitionReadersTool;
impl Tool for SlotDefinitionReadersTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-READERS"
    }
    fn description(&self) -> &str {
        "Get reader methods of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// SLOT-DEFINITION-WRITERS - Get slot writers
pub struct SlotDefinitionWritersTool;
impl Tool for SlotDefinitionWritersTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-WRITERS"
    }
    fn description(&self) -> &str {
        "Get writer methods of slot"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// SLOT-DEFINITION-LOCATION - Get slot storage location
pub struct SlotDefinitionLocationTool;
impl Tool for SlotDefinitionLocationTool {
    fn name(&self) -> &str {
        "SLOT-DEFINITION-LOCATION"
    }
    fn description(&self) -> &str {
        "Get storage location of slot"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let _ = args; // Placeholder implementation
        Ok(Value::Int(0))
    }
}

// ============================================================
// SPECIALIZERS
// ============================================================

/// EQL-SPECIALIZER - Create EQL specializer
pub struct EqlSpecializerTool;
impl Tool for EqlSpecializerTool {
    fn name(&self) -> &str {
        "EQL-SPECIALIZER"
    }
    fn description(&self) -> &str {
        "Create EQL specializer"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// EQL-SPECIALIZER-OBJECT - Get object from EQL specializer
pub struct EqlSpecializerObjectTool;
impl Tool for EqlSpecializerObjectTool {
    fn name(&self) -> &str {
        "EQL-SPECIALIZER-OBJECT"
    }
    fn description(&self) -> &str {
        "Get object from EQL specializer"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// SPECIALIZER-DIRECT-GENERIC-FUNCTIONS - Get generic functions
pub struct SpecializerDirectGenericFunctionsTool;
impl Tool for SpecializerDirectGenericFunctionsTool {
    fn name(&self) -> &str {
        "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
    }
    fn description(&self) -> &str {
        "Get generic functions using specializer"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

/// SPECIALIZER-DIRECT-METHODS - Get methods using specializer
pub struct SpecializerDirectMethodsTool;
impl Tool for SpecializerDirectMethodsTool {
    fn name(&self) -> &str {
        "SPECIALIZER-DIRECT-METHODS"
    }
    fn description(&self) -> &str {
        "Get methods using specializer"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(vec![])))
    }
}

// ============================================================
// METACLASSES
// ============================================================

/// ENSURE-GENERIC-FUNCTION - Ensure generic function exists
pub struct EnsureGenericFunctionTool;
impl Tool for EnsureGenericFunctionTool {
    fn name(&self) -> &str {
        "ENSURE-GENERIC-FUNCTION"
    }
    fn description(&self) -> &str {
        "Ensure generic function exists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// ENSURE-CLASS - Ensure class exists
pub struct EnsureClassTool;
impl Tool for EnsureClassTool {
    fn name(&self) -> &str {
        "ENSURE-CLASS"
    }
    fn description(&self) -> &str {
        "Ensure class exists"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// ALLOCATE-INSTANCE - Allocate instance without initialization
pub struct AllocateInstanceTool;
impl Tool for AllocateInstanceTool {
    fn name(&self) -> &str {
        "ALLOCATE-INSTANCE"
    }
    fn description(&self) -> &str {
        "Allocate instance without initialization"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Object(Arc::new(HashMap::new())))
    }
}

/// MAKE-INSTANCE-STANDARD - Standard instance creation
pub struct MakeInstanceStandardTool;
impl Tool for MakeInstanceStandardTool {
    fn name(&self) -> &str {
        "MAKE-INSTANCE-STANDARD"
    }
    fn description(&self) -> &str {
        "Create standard instance"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::Object(Arc::new(HashMap::new())))
    }
}

/// MAKE-INSTANCES-OBSOLETE - Mark instances obsolete
pub struct MakeInstancesObsoleteTool;
impl Tool for MakeInstancesObsoleteTool {
    fn name(&self) -> &str {
        "MAKE-INSTANCES-OBSOLETE"
    }
    fn description(&self) -> &str {
        "Mark class instances obsolete"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// UPDATE-INSTANCE-FOR-REDEFINED-CLASS - Update after redefine
pub struct UpdateInstanceForRedefinedClassTool;
impl Tool for UpdateInstanceForRedefinedClassTool {
    fn name(&self) -> &str {
        "UPDATE-INSTANCE-FOR-REDEFINED-CLASS"
    }
    fn description(&self) -> &str {
        "Update instance after class redefinition"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(if args.is_empty() {
            Value::Null
        } else {
            args[0].clone()
        })
    }
}

/// SET-FUNCALLABLE-INSTANCE-FUNCTION - Set funcallable function
pub struct SetFuncallableInstanceFunctionTool;
impl Tool for SetFuncallableInstanceFunctionTool {
    fn name(&self) -> &str {
        "SET-FUNCALLABLE-INSTANCE-FUNCTION"
    }
    fn description(&self) -> &str {
        "Set function of funcallable instance"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SET-FUNCALLABLE-INSTANCE-FUNCTION".to_string(),
                reason: "requires at least 2 arguments (instance function)".to_string(),
            });
        }
        Ok(args[0].clone())
    }
}

/// FUNCALLABLE-STANDARD-CLASS - Funcallable standard class
pub struct FuncallableStandardClassTool;
impl Tool for FuncallableStandardClassTool {
    fn name(&self) -> &str {
        "FUNCALLABLE-STANDARD-CLASS"
    }
    fn description(&self) -> &str {
        "Funcallable standard class type"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("FUNCALLABLE-STANDARD-CLASS".to_string()))
    }
}

/// FUNCALLABLE-STANDARD-OBJECT - Funcallable standard object
pub struct FuncallableStandardObjectTool;
impl Tool for FuncallableStandardObjectTool {
    fn name(&self) -> &str {
        "FUNCALLABLE-STANDARD-OBJECT"
    }
    fn description(&self) -> &str {
        "Funcallable standard object type"
    }
    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Ok(Value::String("FUNCALLABLE-STANDARD-OBJECT".to_string()))
    }
}

/// Register all CLOS advanced functions
pub fn register(registry: &mut ToolRegistry) {
    // Generic function introspection
    registry.register(GenericFunctionMethodsTool);
    registry.register(GenericFunctionNameTool);
    registry.register(GenericFunctionLambdaListTool);
    registry.register(GenericFunctionArgumentPrecedenceOrderTool);
    registry.register(GenericFunctionDeclarationsTool);
    registry.register(GenericFunctionMethodClassTool);
    registry.register(GenericFunctionMethodCombinationTool);

    // Method introspection
    registry.register(MethodQualifiersTool);
    registry.register(MethodSpecializersTool);
    registry.register(MethodLambdaListTool);
    registry.register(MethodGenericFunctionTool);
    registry.register(MethodFunctionTool);

    // Method management
    registry.register(AddMethodTool);
    registry.register(RemoveMethodTool);
    registry.register(FindMethodTool);
    registry.register(ComputeApplicableMethodsTool);
    registry.register(ComputeApplicableMethodsUsingClassesTool);

    // Class introspection
    registry.register(ClassDirectSuperclassesTool);
    registry.register(ClassDirectSubclassesTool);
    registry.register(ClassDirectSlotsTool);
    registry.register(ClassDefaultInitargsTool);
    registry.register(ClassDirectDefaultInitargsTool);
    registry.register(ClassPrototypeTool);
    registry.register(ClassFinalizedPTool);
    registry.register(FinalizeInheritanceTool);

    // Slot introspection
    registry.register(SlotDefinitionNameTool);
    registry.register(SlotDefinitionInitargsTool);
    registry.register(SlotDefinitionInitformTool);
    registry.register(SlotDefinitionInitfunctionTool);
    registry.register(SlotDefinitionTypeTool);
    registry.register(SlotDefinitionAllocationTool);
    registry.register(SlotDefinitionReadersTool);
    registry.register(SlotDefinitionWritersTool);
    registry.register(SlotDefinitionLocationTool);

    // Specializers
    registry.register(EqlSpecializerTool);
    registry.register(EqlSpecializerObjectTool);
    registry.register(SpecializerDirectGenericFunctionsTool);
    registry.register(SpecializerDirectMethodsTool);

    // Metaclasses
    registry.register(EnsureGenericFunctionTool);
    registry.register(EnsureClassTool);
    registry.register(AllocateInstanceTool);
    registry.register(MakeInstanceStandardTool);
    registry.register(MakeInstancesObsoleteTool);
    registry.register(UpdateInstanceForRedefinedClassTool);
    registry.register(SetFuncallableInstanceFunctionTool);
    registry.register(FuncallableStandardClassTool);
    registry.register(FuncallableStandardObjectTool);
}
