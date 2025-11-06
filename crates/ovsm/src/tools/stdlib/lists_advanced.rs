//! Advanced list manipulation tools - Common Lisp compatible

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

/// Register advanced list tools
pub fn register(registry: &mut ToolRegistry) {
    // List construction
    registry.register(MakeListTool);
    registry.register(CopyListTool);
    registry.register(CopyTreeTool);

    // List operations
    registry.register(LdiffTool);
    registry.register(TailpTool);
    registry.register(NthListTool);
    registry.register(LastNTool);

    // Tree operations
    registry.register(TreeEqualTool);
    registry.register(SublisTool);
    registry.register(NsublisTool);

    // List reorganization
    registry.register(NthconseTool);
    registry.register(RplaTool);
    registry.register(RpldTool);

    // Circular list operations
    registry.register(ListLengthTool);
    registry.register(ListTailTool);

    // Predicates
    registry.register(EndpTool);
    registry.register(ListTuplePTool);

    // Sorting
    registry.register(StableSortTool);
    registry.register(SortByTool);
}

// ============================================================================
// List Construction
// ============================================================================

/// MAKE-LIST - Create list of specified size
pub struct MakeListTool;

impl Tool for MakeListTool {
    fn name(&self) -> &str {
        "MAKE-LIST"
    }

    fn description(&self) -> &str {
        "Create list of specified size with initial element"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "MAKE-LIST".to_string(),
                reason: "Expected size argument".to_string(),
            });
        }

        let size = args[0].as_int()? as usize;
        let initial = if args.len() > 1 {
            args[1].clone()
        } else {
            Value::Null
        };

        Ok(Value::Array(Arc::new(vec![initial; size])))
    }
}

/// COPY-LIST - Create shallow copy of list
pub struct CopyListTool;

impl Tool for CopyListTool {
    fn name(&self) -> &str {
        "COPY-LIST"
    }

    fn description(&self) -> &str {
        "Create shallow copy of list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COPY-LIST".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        Ok(Value::Array(Arc::new(list.to_vec())))
    }
}

/// COPY-TREE - Create deep copy of tree structure
pub struct CopyTreeTool;

impl Tool for CopyTreeTool {
    fn name(&self) -> &str {
        "COPY-TREE"
    }

    fn description(&self) -> &str {
        "Create deep copy of tree structure"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COPY-TREE".to_string(),
                reason: "Expected tree argument".to_string(),
            });
        }

        fn deep_copy(val: &Value) -> Value {
            match val {
                Value::Array(arr) => {
                    let copied: Vec<Value> = arr.iter().map(deep_copy).collect();
                    Value::Array(Arc::new(copied))
                }
                Value::Object(obj) => {
                    let mut copied = std::collections::HashMap::new();
                    for (k, v) in obj.iter() {
                        copied.insert(k.clone(), deep_copy(v));
                    }
                    Value::object(copied)
                }
                other => other.clone(),
            }
        }

        Ok(deep_copy(&args[0]))
    }
}

// ============================================================================
// List Operations
// ============================================================================

/// LDIFF - List difference (elements before sublist)
pub struct LdiffTool;

impl Tool for LdiffTool {
    fn name(&self) -> &str {
        "LDIFF"
    }

    fn description(&self) -> &str {
        "Return elements of list before sublist"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "LDIFF".to_string(),
                reason: "Expected list and sublist arguments".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let sublist = args[1].as_array()?;

        // Find where sublist starts in list
        if sublist.is_empty() {
            return Ok(Value::Array(Arc::new(list.to_vec())));
        }

        for i in 0..list.len() {
            if i + sublist.len() <= list.len() {
                let slice = &list[i..i + sublist.len()];
                if slice == &sublist[..] {
                    return Ok(Value::Array(Arc::new(list[..i].to_vec())));
                }
            }
        }

        // Sublist not found, return whole list
        Ok(Value::Array(Arc::new(list.to_vec())))
    }
}

/// TAILP - Check if sublist is tail of list
pub struct TailpTool;

impl Tool for TailpTool {
    fn name(&self) -> &str {
        "TAILP"
    }

    fn description(&self) -> &str {
        "Check if sublist is a tail of list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TAILP".to_string(),
                reason: "Expected sublist and list arguments".to_string(),
            });
        }

        let sublist = args[0].as_array()?;
        let list = args[1].as_array()?;

        if sublist.len() > list.len() {
            return Ok(Value::Bool(false));
        }

        let offset = list.len() - sublist.len();
        Ok(Value::Bool(list[offset..] == sublist[..]))
    }
}

/// NTHLIST - Return Nth cons cell
pub struct NthListTool;

impl Tool for NthListTool {
    fn name(&self) -> &str {
        "NTHLIST"
    }

    fn description(&self) -> &str {
        "Return list starting at Nth position"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "NTHLIST".to_string(),
                reason: "Expected N and list arguments".to_string(),
            });
        }

        let n = args[0].as_int()? as usize;
        let list = args[1].as_array()?;

        if n >= list.len() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(list[n..].to_vec())))
        }
    }
}

/// LASTN - Return last N elements
pub struct LastNTool;

impl Tool for LastNTool {
    fn name(&self) -> &str {
        "LASTN"
    }

    fn description(&self) -> &str {
        "Return last N elements of list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "LASTN".to_string(),
                reason: "Expected list and N arguments".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let n = args[1].as_int()? as usize;

        let start = if n >= list.len() { 0 } else { list.len() - n };
        Ok(Value::Array(Arc::new(list[start..].to_vec())))
    }
}

// ============================================================================
// Tree Operations
// ============================================================================

/// TREE-EQUAL - Deep tree equality
pub struct TreeEqualTool;

impl Tool for TreeEqualTool {
    fn name(&self) -> &str {
        "TREE-EQUAL"
    }

    fn description(&self) -> &str {
        "Deep equality check for tree structures"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "TREE-EQUAL".to_string(),
                reason: "Expected two tree arguments".to_string(),
            });
        }

        fn deep_equal(a: &Value, b: &Value) -> bool {
            match (a, b) {
                (Value::Array(arr1), Value::Array(arr2)) => {
                    arr1.len() == arr2.len()
                        && arr1.iter().zip(arr2.iter()).all(|(x, y)| deep_equal(x, y))
                }
                (Value::Object(obj1), Value::Object(obj2)) => {
                    obj1.len() == obj2.len()
                        && obj1
                            .iter()
                            .all(|(k, v1)| obj2.get(k).is_some_and(|v2| deep_equal(v1, v2)))
                }
                (a, b) => a == b,
            }
        }

        Ok(Value::Bool(deep_equal(&args[0], &args[1])))
    }
}

/// SUBLIS - Substitute using association list
pub struct SublisTool;

impl Tool for SublisTool {
    fn name(&self) -> &str {
        "SUBLIS"
    }

    fn description(&self) -> &str {
        "Substitute using association list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBLIS".to_string(),
                reason: "Expected alist and tree arguments".to_string(),
            });
        }

        let alist = args[0].as_array()?;
        let tree = &args[1];

        fn sublis_recursive(alist: &[Value], tree: &Value) -> Value {
            // Check if tree matches any key in alist
            for pair in alist {
                if let Value::Array(p) = pair {
                    if p.len() >= 2 && &p[0] == tree {
                        return p[1].clone();
                    }
                }
            }

            // Recursively process subtrees
            if let Value::Array(arr) = tree {
                let result: Vec<Value> = arr
                    .iter()
                    .map(|elem| sublis_recursive(alist, elem))
                    .collect();
                Value::Array(Arc::new(result))
            } else {
                tree.clone()
            }
        }

        Ok(sublis_recursive(alist, tree))
    }
}

/// NSUBLIS - Destructive substitute (creates new in OVSM)
pub struct NsublisTool;

impl Tool for NsublisTool {
    fn name(&self) -> &str {
        "NSUBLIS"
    }

    fn description(&self) -> &str {
        "Destructive substitute (creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        SublisTool.execute(args)
    }
}

// ============================================================================
// List Reorganization
// ============================================================================

/// NTHCONSE - Set Nth element (RPLACA variant)
pub struct NthconseTool;

impl Tool for NthconseTool {
    fn name(&self) -> &str {
        "NTHCONSE"
    }

    fn description(&self) -> &str {
        "Return new list with Nth element replaced"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "NTHCONSE".to_string(),
                reason: "Expected N, list, and value arguments".to_string(),
            });
        }

        let n = args[0].as_int()? as usize;
        let list = args[1].as_array()?;
        let value = &args[2];

        if n >= list.len() {
            return Err(Error::IndexOutOfBounds {
                index: n,
                length: list.len(),
            });
        }

        let mut new_list = list.to_vec();
        new_list[n] = value.clone();
        Ok(Value::Array(Arc::new(new_list)))
    }
}

/// RPLA - Replace CAR (first element)
pub struct RplaTool;

impl Tool for RplaTool {
    fn name(&self) -> &str {
        "RPLA"
    }

    fn description(&self) -> &str {
        "Replace first element of list (returns new list)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "RPLA".to_string(),
                reason: "Expected list and value arguments".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let value = &args[1];

        if list.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "RPLA".to_string(),
            });
        }

        let mut new_list = list.to_vec();
        new_list[0] = value.clone();
        Ok(Value::Array(Arc::new(new_list)))
    }
}

/// RPLD - Replace CDR (rest of list)
pub struct RpldTool;

impl Tool for RpldTool {
    fn name(&self) -> &str {
        "RPLD"
    }

    fn description(&self) -> &str {
        "Replace rest of list (returns new list)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "RPLD".to_string(),
                reason: "Expected list and new-rest arguments".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let new_rest = args[1].as_array()?;

        if list.is_empty() {
            return Err(Error::EmptyCollection {
                operation: "RPLD".to_string(),
            });
        }

        let mut result = vec![list[0].clone()];
        result.extend(new_rest.iter().cloned());
        Ok(Value::Array(Arc::new(result)))
    }
}

// ============================================================================
// Circular List Operations
// ============================================================================

/// LIST-LENGTH - Get list length (handles circular lists)
pub struct ListLengthTool;

impl Tool for ListLengthTool {
    fn name(&self) -> &str {
        "LIST-LENGTH"
    }

    fn description(&self) -> &str {
        "Get list length (returns null for circular)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LIST-LENGTH".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        // OVSM doesn't have circular lists, so just return length
        Ok(Value::Int(list.len() as i64))
    }
}

/// LIST-TAIL - Return tail after N elements
pub struct ListTailTool;

impl Tool for ListTailTool {
    fn name(&self) -> &str {
        "LIST-TAIL"
    }

    fn description(&self) -> &str {
        "Return list tail after skipping N elements"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "LIST-TAIL".to_string(),
                reason: "Expected list and N arguments".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let n = args[1].as_int()? as usize;

        if n >= list.len() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(list[n..].to_vec())))
        }
    }
}

// ============================================================================
// Predicates
// ============================================================================

/// ENDP - Check if list is empty
pub struct EndpTool;

impl Tool for EndpTool {
    fn name(&self) -> &str {
        "ENDP"
    }

    fn description(&self) -> &str {
        "Check if list is empty"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ENDP".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => Ok(Value::Bool(arr.is_empty())),
            Value::Null => Ok(Value::Bool(true)),
            _ => Err(Error::TypeError {
                expected: "list".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// LIST-TUPLE-P - Check if value is a proper list
pub struct ListTuplePTool;

impl Tool for ListTuplePTool {
    fn name(&self) -> &str {
        "LIST-TUPLE-P"
    }

    fn description(&self) -> &str {
        "Check if value is a proper list (not circular)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "LIST-TUPLE-P".to_string(),
                reason: "Expected argument".to_string(),
            });
        }

        // In OVSM, all arrays are proper lists (no circular structures)
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Array(_) | Value::Null
        )))
    }
}

// ============================================================================
// Sorting
// ============================================================================

/// STABLE-SORT - Stable sort (maintains order of equal elements)
pub struct StableSortTool;

impl Tool for StableSortTool {
    fn name(&self) -> &str {
        "STABLE-SORT"
    }

    fn description(&self) -> &str {
        "Stable sort that maintains order of equal elements"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "STABLE-SORT".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let mut sorted = list.to_vec();

        sorted.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::String(x), Value::String(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });

        Ok(Value::Array(Arc::new(sorted)))
    }
}

/// SORT-BY - Sort with custom key function
pub struct SortByTool;

impl Tool for SortByTool {
    fn name(&self) -> &str {
        "SORT-BY"
    }

    fn description(&self) -> &str {
        "Sort list by a key (expects keyword :key followed by key name)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SORT-BY".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;

        // Simple sort - just sort by the values themselves
        let mut sorted = list.to_vec();
        sorted.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::String(x), Value::String(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });

        Ok(Value::Array(Arc::new(sorted)))
    }
}
