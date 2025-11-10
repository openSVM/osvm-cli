//! Array and vector manipulation tools - Common Lisp compatible

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

/// Register all array manipulation tools
pub fn register(registry: &mut ToolRegistry) {
    // Array creation
    registry.register(MakeArrayTool);
    registry.register(VectorTool);
    registry.register(MakeSequenceTool);

    // Array access
    registry.register(ArefTool);
    registry.register(SvrefTool);
    registry.register(RowMajorArefTool);

    // Array properties
    registry.register(ArrayDimensionsTool);
    registry.register(ArrayRankTool);
    registry.register(ArrayTotalSizeTool);
    registry.register(ArrayInBoundsPTool);

    // Array operations
    registry.register(AdjustArrayTool);
    registry.register(ArrayElementTypeTool);

    // More CAR/CDR combinations (complete the set)
    registry.register(CaddrTool);
    registry.register(CdadrTool);
    registry.register(CadadrTool);
    registry.register(CddarTool);
    registry.register(CaadrTool);
    registry.register(CadddrTool);
    registry.register(CdaddrTool);
    registry.register(CdddrTool);
    registry.register(CaaaarTool);
    registry.register(CdaaarTool);
    registry.register(CadaarTool);
    registry.register(CddaarTool);
    registry.register(CaadarTool);
}

// ============================================================================
// Array Creation
// ============================================================================

/// MAKE-ARRAY - Create array with specified dimensions
pub struct MakeArrayTool;

impl Tool for MakeArrayTool {
    fn name(&self) -> &str {
        "MAKE-ARRAY"
    }

    fn description(&self) -> &str {
        "Create array with specified size and optional initial value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-ARRAY".to_string(),
                reason: "Expected size argument".to_string(),
            });
        }

        let size = args[0].as_int()? as usize;
        let initial_element = if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        };

        let array = vec![initial_element; size];
        Ok(Value::Array(Arc::new(array)))
    }
}

/// VECTOR - Create vector from arguments
pub struct VectorTool;

impl Tool for VectorTool {
    fn name(&self) -> &str {
        "VECTOR"
    }

    fn description(&self) -> &str {
        "Create vector from arguments (same as LIST)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        Ok(Value::Array(Arc::new(args.to_vec())))
    }
}

/// MAKE-SEQUENCE - Create sequence of specified type and size
pub struct MakeSequenceTool;

impl Tool for MakeSequenceTool {
    fn name(&self) -> &str {
        "MAKE-SEQUENCE"
    }

    fn description(&self) -> &str {
        "Create sequence of specified size with optional initial element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-SEQUENCE".to_string(),
                reason: "Expected size argument".to_string(),
            });
        }

        let size = args[0].as_int()? as usize;
        let initial_element = if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        };

        let seq = vec![initial_element; size];
        Ok(Value::Array(Arc::new(seq)))
    }
}

// ============================================================================
// Array Access
// ============================================================================

/// AREF - Access array element
pub struct ArefTool;

impl Tool for ArefTool {
    fn name(&self) -> &str {
        "AREF"
    }

    fn description(&self) -> &str {
        "Access array element at index"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "AREF".to_string(),
                reason: "Expected array and index".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let index = args[1].as_int()? as usize;

        array.get(index).cloned().ok_or(Error::IndexOutOfBounds {
            index,
            length: array.len(),
        })
    }
}

/// SVREF - Simple vector ref (same as AREF in OVSM)
pub struct SvrefTool;

impl Tool for SvrefTool {
    fn name(&self) -> &str {
        "SVREF"
    }

    fn description(&self) -> &str {
        "Access simple vector element at index"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        ArefTool.execute(args)
    }
}

/// ROW-MAJOR-AREF - Row-major array access
pub struct RowMajorArefTool;

impl Tool for RowMajorArefTool {
    fn name(&self) -> &str {
        "ROW-MAJOR-AREF"
    }

    fn description(&self) -> &str {
        "Access array element using row-major index"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // In OVSM, arrays are 1D, so this is same as AREF
        ArefTool.execute(args)
    }
}

// ============================================================================
// Array Properties
// ============================================================================

/// ARRAY-DIMENSIONS - Get array dimensions
pub struct ArrayDimensionsTool;

impl Tool for ArrayDimensionsTool {
    fn name(&self) -> &str {
        "ARRAY-DIMENSIONS"
    }

    fn description(&self) -> &str {
        "Get array dimensions as list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ARRAY-DIMENSIONS".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let array = args[0].as_array()?;
        Ok(Value::Array(Arc::new(vec![Value::Int(array.len() as i64)])))
    }
}

/// ARRAY-RANK - Get array rank (number of dimensions)
pub struct ArrayRankTool;

impl Tool for ArrayRankTool {
    fn name(&self) -> &str {
        "ARRAY-RANK"
    }

    fn description(&self) -> &str {
        "Get number of dimensions (always 1 in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ARRAY-RANK".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        args[0].as_array()?;
        Ok(Value::Int(1)) // Always 1D in OVSM
    }
}

/// ARRAY-TOTAL-SIZE - Get total number of elements
pub struct ArrayTotalSizeTool;

impl Tool for ArrayTotalSizeTool {
    fn name(&self) -> &str {
        "ARRAY-TOTAL-SIZE"
    }

    fn description(&self) -> &str {
        "Get total number of elements in array"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ARRAY-TOTAL-SIZE".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        let array = args[0].as_array()?;
        Ok(Value::Int(array.len() as i64))
    }
}

/// ARRAY-IN-BOUNDS-P - Check if indices are valid
pub struct ArrayInBoundsPTool;

impl Tool for ArrayInBoundsPTool {
    fn name(&self) -> &str {
        "ARRAY-IN-BOUNDS-P"
    }

    fn description(&self) -> &str {
        "Check if index is within bounds"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ARRAY-IN-BOUNDS-P".to_string(),
                reason: "Expected array and index".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let index = args[1].as_int()? as usize;

        Ok(Value::Bool(index < array.len()))
    }
}

// ============================================================================
// Array Operations
// ============================================================================

/// ADJUST-ARRAY - Adjust array to new size
pub struct AdjustArrayTool;

impl Tool for AdjustArrayTool {
    fn name(&self) -> &str {
        "ADJUST-ARRAY"
    }

    fn description(&self) -> &str {
        "Adjust array to new size (creates new array in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ADJUST-ARRAY".to_string(),
                reason: "Expected array and new size".to_string(),
            });
        }

        let array = args[0].as_array()?;
        let new_size = args[1].as_int()? as usize;

        let mut new_array = Vec::with_capacity(new_size);

        // Copy existing elements
        for i in 0..new_size {
            if i < array.len() {
                new_array.push(array[i].clone());
            } else {
                new_array.push(Value::Null);
            }
        }

        Ok(Value::Array(Arc::new(new_array)))
    }
}

/// ARRAY-ELEMENT-TYPE - Get array element type
pub struct ArrayElementTypeTool;

impl Tool for ArrayElementTypeTool {
    fn name(&self) -> &str {
        "ARRAY-ELEMENT-TYPE"
    }

    fn description(&self) -> &str {
        "Get array element type (returns 'T' for any in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ARRAY-ELEMENT-TYPE".to_string(),
                reason: "Expected array argument".to_string(),
            });
        }

        args[0].as_array()?;
        Ok(Value::String("T".to_string())) // T means any type
    }
}

// ============================================================================
// Additional CAR/CDR Combinations
// ============================================================================

/// CADDR - Get third element
pub struct CaddrTool;

impl Tool for CaddrTool {
    fn name(&self) -> &str {
        "CADDR"
    }

    fn description(&self) -> &str {
        "Get third element of list (CAR of CDDR)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CADDR".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 3 {
            return Err(Error::IndexOutOfBounds {
                index: 2,
                length: list.len(),
            });
        }
        Ok(list[2].clone())
    }
}

/// CDADR - CDR of CADR
pub struct CdadrTool;

impl Tool for CdadrTool {
    fn name(&self) -> &str {
        "CDADR"
    }

    fn description(&self) -> &str {
        "Get CDR of second element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDADR".to_string(),
                reason: "Expected nested list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 2 {
            return Err(Error::IndexOutOfBounds {
                index: 1,
                length: list.len(),
            });
        }

        let second = list[1].as_array()?;
        if second.is_empty() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(second[1..].to_vec())))
        }
    }
}

/// CADADR - CAR of CDADR
pub struct CadadrTool;

impl Tool for CadadrTool {
    fn name(&self) -> &str {
        "CADADR"
    }

    fn description(&self) -> &str {
        "Get CAR of CDR of CADR"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CADADR".to_string(),
                reason: "Expected nested list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 2 {
            return Err(Error::EmptyCollection {
                operation: "CADADR".to_string(),
            });
        }

        let second = list[1].as_array()?;
        if second.len() < 2 {
            return Err(Error::EmptyCollection {
                operation: "CADADR".to_string(),
            });
        }

        Ok(second[1].clone())
    }
}

/// CDDAR - CDR of CDAR
pub struct CddarTool;

impl Tool for CddarTool {
    fn name(&self) -> &str {
        "CDDAR"
    }

    fn description(&self) -> &str {
        "Get CDR of CDR of CAR"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDDAR".to_string(),
                reason: "Expected nested list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "CDDAR".to_string(),
            });
        }

        let first = list[0].as_array()?;
        if first.len() < 2 {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(first[2..].to_vec())))
        }
    }
}

/// CAADR - CAR of CADR
pub struct CaadrTool;

impl Tool for CaadrTool {
    fn name(&self) -> &str {
        "CAADR"
    }

    fn description(&self) -> &str {
        "Get first element of second element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CAADR".to_string(),
                reason: "Expected nested list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 2 {
            return Err(Error::IndexOutOfBounds {
                index: 1,
                length: list.len(),
            });
        }

        let second = list[1].as_array()?;
        second
            .first()
            .cloned()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAADR".to_string(),
            })
    }
}

/// CADDDR - Get fourth element
pub struct CadddrTool;

impl Tool for CadddrTool {
    fn name(&self) -> &str {
        "CADDDR"
    }

    fn description(&self) -> &str {
        "Get fourth element of list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CADDDR".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 4 {
            return Err(Error::IndexOutOfBounds {
                index: 3,
                length: list.len(),
            });
        }
        Ok(list[3].clone())
    }
}

/// CDADDR - CDR of CADDR
pub struct CdaddrTool;

impl Tool for CdaddrTool {
    fn name(&self) -> &str {
        "CDADDR"
    }

    fn description(&self) -> &str {
        "Get CDR of third element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDADDR".to_string(),
                reason: "Expected nested list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 3 {
            return Err(Error::IndexOutOfBounds {
                index: 2,
                length: list.len(),
            });
        }

        let third = list[2].as_array()?;
        if third.is_empty() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(third[1..].to_vec())))
        }
    }
}

/// CDDDR - CDR of CDDR (skip first 3)
pub struct CdddrTool;

impl Tool for CdddrTool {
    fn name(&self) -> &str {
        "CDDDR"
    }

    fn description(&self) -> &str {
        "Get all elements after first three"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDDDR".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        if list.len() < 3 {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(list[3..].to_vec())))
        }
    }
}

// 4-level CAR/CDR combinations

/// CAAAAR - CAR of CAR of CAR of CAR
pub struct CaaaarTool;

impl Tool for CaaaarTool {
    fn name(&self) -> &str {
        "CAAAAR"
    }

    fn description(&self) -> &str {
        "Get first of first of first of first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CAAAAR".to_string(),
                reason: "Expected deeply nested list".to_string(),
            });
        }

        let l1 = args[0]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAAAAR".to_string(),
            })?;
        let l2 = l1
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAAAAR".to_string(),
            })?;
        let l3 = l2
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAAAAR".to_string(),
            })?;
        let l4 = l3
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAAAAR".to_string(),
            })?;

        Ok(l4.clone())
    }
}

/// CDAAAR - CDR of CAR of CAR of CAR
pub struct CdaaarTool;

impl Tool for CdaaarTool {
    fn name(&self) -> &str {
        "CDAAAR"
    }

    fn description(&self) -> &str {
        "Get rest of first of first of first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDAAAR".to_string(),
                reason: "Expected deeply nested list".to_string(),
            });
        }

        let l1 = args[0]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CDAAAR".to_string(),
            })?;
        let l2 = l1
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CDAAAR".to_string(),
            })?;
        let l3 = l2
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CDAAAR".to_string(),
            })?;
        let l4 = l3.as_array()?;

        if l4.is_empty() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(l4[1..].to_vec())))
        }
    }
}

/// CADAAR - CAR of CDR of CAR of CAR
pub struct CadaarTool;

impl Tool for CadaarTool {
    fn name(&self) -> &str {
        "CADAAR"
    }

    fn description(&self) -> &str {
        "Get first of rest of first of first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CADAAR".to_string(),
                reason: "Expected deeply nested list".to_string(),
            });
        }

        let l1 = args[0]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CADAAR".to_string(),
            })?;
        let l2 = l1
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CADAAR".to_string(),
            })?;
        let l3 = l2.as_array()?;

        if l3.len() < 2 {
            return Err(Error::IndexOutOfBounds {
                index: 1,
                length: l3.len(),
            });
        }

        Ok(l3[1].clone())
    }
}

/// CDDAAR - CDR of CDR of CAR of CAR
pub struct CddaarTool;

impl Tool for CddaarTool {
    fn name(&self) -> &str {
        "CDDAAR"
    }

    fn description(&self) -> &str {
        "Get rest of rest of first of first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CDDAAR".to_string(),
                reason: "Expected deeply nested list".to_string(),
            });
        }

        let l1 = args[0]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CDDAAR".to_string(),
            })?;
        let l2 = l1
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CDDAAR".to_string(),
            })?;
        let l3 = l2.as_array()?;

        if l3.len() < 2 {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(l3[2..].to_vec())))
        }
    }
}

/// CAADAR - CAR of CAR of CDR of CAR
pub struct CaadarTool;

impl Tool for CaadarTool {
    fn name(&self) -> &str {
        "CAADAR"
    }

    fn description(&self) -> &str {
        "Get first of first of rest of first"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CAADAR".to_string(),
                reason: "Expected deeply nested list".to_string(),
            });
        }

        let l1 = args[0]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAADAR".to_string(),
            })?;
        let l2 = l1.as_array()?;

        if l2.len() < 2 {
            return Err(Error::IndexOutOfBounds {
                index: 1,
                length: l2.len(),
            });
        }

        let l3 = l2[1]
            .as_array()?
            .first()
            .ok_or_else(|| Error::EmptyCollection {
                operation: "CAADAR".to_string(),
            })?;

        Ok(l3.clone())
    }
}
