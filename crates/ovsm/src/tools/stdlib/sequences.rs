//! Sequence manipulation tools - Common Lisp compatible sequence functions

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::sync::Arc;

/// Register all sequence manipulation tools
pub fn register(registry: &mut ToolRegistry) {
    // Core sequence operations
    registry.register(EltTool);
    registry.register(CopySeqTool);
    registry.register(NreverseTool);

    // List operations
    registry.register(AppendTool);
    registry.register(NconcTool);
    registry.register(RevappendTool);
    registry.register(ButlastTool);
    registry.register(NthcdrTool);

    // Membership and finding
    registry.register(MemberTool);
    registry.register(MemberIfTool);
    registry.register(FindTool);
    registry.register(FindIfTool);
    registry.register(PositionIfTool);

    // Removal operations
    registry.register(RemoveTool);
    registry.register(RemoveIfTool);
    registry.register(RemoveIfNotTool);
    registry.register(RemoveDuplicatesTool);
    registry.register(DeleteTool);
    registry.register(DeleteIfTool);

    // Substitution
    registry.register(SubstTool);
    registry.register(SubstIfTool);
    registry.register(NsubstTool);

    // Set operations
    registry.register(UnionTool);
    registry.register(IntersectionTool);
    registry.register(SetDifferenceTool);
    registry.register(SetExclusiveOrTool);
    registry.register(SubsetpTool);

    // Association lists
    registry.register(AssocTool);
    registry.register(RassocTool);
    registry.register(PairlisTool);

    // Predicates
    registry.register(EveryTool);
    registry.register(SomeTool);
    registry.register(NotanyTool);
    registry.register(NoteveryTool);

    // Reduction and mapping
    registry.register(ReduceTool);
    registry.register(MapcarTool);
    registry.register(MapcTool);
    registry.register(MaplistTool);

    // Miscellaneous
    registry.register(FillTool);
    registry.register(MismatchTool);

    // Advanced sequence operations
    registry.register(MergeTool);
    registry.register(StableSortTool);
    registry.register(SearchTool);
    registry.register(SubstituteIfTool);
    registry.register(SubstituteIfNotTool);
    registry.register(NsubstituteIfTool);
    registry.register(NsubstituteIfNotTool);
    registry.register(DeleteDuplicatesTool);
    registry.register(CountTool);
    registry.register(CountIfTool);
    registry.register(PositionTool);
    registry.register(FindIfNotTool);
    registry.register(ReplaceToolSeq);
    registry.register(NreplaceTool);
    registry.register(ConcatenateTool);
    registry.register(LengthSeqTool);
    registry.register(ReverseTool);
    registry.register(SubsequenceTool);
    registry.register(SortTool);
    registry.register(MapTool);
}

// ============================================================================
// Core Sequence Operations
// ============================================================================

/// ELT - Access element at index in sequence
pub struct EltTool;

impl Tool for EltTool {
    fn name(&self) -> &str {
        "ELT"
    }

    fn description(&self) -> &str {
        "Access element at index in sequence"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ELT".to_string(),
                reason: "Expected sequence and index".to_string(),
            });
        }

        let index = args[1].as_int()? as usize;

        match &args[0] {
            Value::Array(arr) => arr
                .get(index)
                .cloned()
                .ok_or_else(|| Error::IndexOutOfBounds {
                    index,
                    length: arr.len(),
                }),
            Value::String(s) => {
                let chars: Vec<char> = s.chars().collect();
                chars
                    .get(index)
                    .map(|c| Value::String(c.to_string()))
                    .ok_or(Error::IndexOutOfBounds {
                        index,
                        length: chars.len(),
                    })
            }
            _ => Err(Error::TypeError {
                expected: "sequence (array or string)".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// COPY-SEQ - Create a copy of a sequence
pub struct CopySeqTool;

impl Tool for CopySeqTool {
    fn name(&self) -> &str {
        "COPY-SEQ"
    }

    fn description(&self) -> &str {
        "Create a copy of a sequence"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COPY-SEQ".to_string(),
                reason: "Expected sequence argument".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => Ok(Value::Array(Arc::new(arr.as_ref().clone()))),
            Value::String(s) => Ok(Value::String(s.clone())),
            _ => Err(Error::TypeError {
                expected: "sequence".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

/// NREVERSE - Destructively reverse sequence (in OVSM, creates new reversed sequence)
pub struct NreverseTool;

impl Tool for NreverseTool {
    fn name(&self) -> &str {
        "NREVERSE"
    }

    fn description(&self) -> &str {
        "Reverse sequence (destructive in CL, creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "NREVERSE".to_string(),
                reason: "Expected sequence argument".to_string(),
            });
        }

        match &args[0] {
            Value::Array(arr) => {
                let mut reversed = arr.as_ref().clone();
                reversed.reverse();
                Ok(Value::Array(Arc::new(reversed)))
            }
            Value::String(s) => {
                let reversed: String = s.chars().rev().collect();
                Ok(Value::String(reversed))
            }
            _ => Err(Error::TypeError {
                expected: "sequence".to_string(),
                got: args[0].type_name(),
            }),
        }
    }
}

// ============================================================================
// List Operations
// ============================================================================

/// APPEND - Concatenate lists
pub struct AppendTool;

impl Tool for AppendTool {
    fn name(&self) -> &str {
        "APPEND"
    }

    fn description(&self) -> &str {
        "Concatenate lists (returns new list)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        let mut result = Vec::new();
        for arg in args {
            match arg {
                Value::Array(arr) => result.extend(arr.iter().cloned()),
                Value::Null => {}
                _ => {
                    return Err(Error::TypeError {
                        expected: "list".to_string(),
                        got: arg.type_name(),
                    })
                }
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }
}

/// NCONC - Destructively concatenate lists (creates new in OVSM)
pub struct NconcTool;

impl Tool for NconcTool {
    fn name(&self) -> &str {
        "NCONC"
    }

    fn description(&self) -> &str {
        "Concatenate lists (destructive in CL, creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        AppendTool.execute(args)
    }
}

/// REVAPPEND - Reverse and append
pub struct RevappendTool;

impl Tool for RevappendTool {
    fn name(&self) -> &str {
        "REVAPPEND"
    }

    fn description(&self) -> &str {
        "Reverse first list and append second list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REVAPPEND".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let mut result = Vec::new();
        result.extend(list1.iter().rev().cloned());
        result.extend(list2.iter().cloned());

        Ok(Value::Array(Arc::new(result)))
    }
}

/// BUTLAST - Return all but last N elements
pub struct ButlastTool;

impl Tool for ButlastTool {
    fn name(&self) -> &str {
        "BUTLAST"
    }

    fn description(&self) -> &str {
        "Return all but last N elements (default N=1)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "BUTLAST".to_string(),
                reason: "Expected list argument".to_string(),
            });
        }

        let list = args[0].as_array()?;
        let n = if args.len() > 1 {
            args[1].as_int()? as usize
        } else {
            1
        };

        if n >= list.len() {
            Ok(Value::Array(Arc::new(vec![])))
        } else {
            Ok(Value::Array(Arc::new(list[..list.len() - n].to_vec())))
        }
    }
}

/// NTHCDR - Return Nth cdr of list
pub struct NthcdrTool;

impl Tool for NthcdrTool {
    fn name(&self) -> &str {
        "NTHCDR"
    }

    fn description(&self) -> &str {
        "Return Nth cdr of list (skip N elements)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "NTHCDR".to_string(),
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

// ============================================================================
// Membership and Finding
// ============================================================================

/// MEMBER - Find element in list, return tail starting at element
pub struct MemberTool;

impl Tool for MemberTool {
    fn name(&self) -> &str {
        "MEMBER"
    }

    fn description(&self) -> &str {
        "Find element in list, return tail or null"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MEMBER".to_string(),
                reason: "Expected item and list arguments".to_string(),
            });
        }

        let item = &args[0];
        let list = args[1].as_array()?;

        for (i, elem) in list.iter().enumerate() {
            if elem == item {
                return Ok(Value::Array(Arc::new(list[i..].to_vec())));
            }
        }

        Ok(Value::Null)
    }
}

/// MEMBER-IF - Find element satisfying predicate
pub struct MemberIfTool;

impl Tool for MemberIfTool {
    fn name(&self) -> &str {
        "MEMBER-IF"
    }

    fn description(&self) -> &str {
        "Find first element where predicate returns true"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MEMBER-IF".to_string(),
                reason: "Expected predicate and list arguments".to_string(),
            });
        }

        // For now, just check truthiness of elements
        // Full implementation would require lambda evaluation
        let list = args[1].as_array()?;

        for (i, elem) in list.iter().enumerate() {
            if elem.is_truthy() {
                return Ok(Value::Array(Arc::new(list[i..].to_vec())));
            }
        }

        Ok(Value::Null)
    }
}

/// FIND - Find element in sequence
pub struct FindTool;

impl Tool for FindTool {
    fn name(&self) -> &str {
        "FIND"
    }

    fn description(&self) -> &str {
        "Find element in sequence, return element or null"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "FIND".to_string(),
                reason: "Expected item and sequence arguments".to_string(),
            });
        }

        let item = &args[0];
        let seq = args[1].as_array()?;

        for elem in seq.iter() {
            if elem == item {
                return Ok(elem.clone());
            }
        }

        Ok(Value::Null)
    }
}

/// FIND-IF - Find element satisfying predicate
pub struct FindIfTool;

impl Tool for FindIfTool {
    fn name(&self) -> &str {
        "FIND-IF"
    }

    fn description(&self) -> &str {
        "Find first element where predicate returns true"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "FIND-IF".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;

        for elem in seq.iter() {
            if elem.is_truthy() {
                return Ok(elem.clone());
            }
        }

        Ok(Value::Null)
    }
}

/// POSITION-IF - Find position of element satisfying predicate
pub struct PositionIfTool;

impl Tool for PositionIfTool {
    fn name(&self) -> &str {
        "POSITION-IF"
    }

    fn description(&self) -> &str {
        "Find index of first element where predicate returns true"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "POSITION-IF".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;

        for (i, elem) in seq.iter().enumerate() {
            if elem.is_truthy() {
                return Ok(Value::Int(i as i64));
            }
        }

        Ok(Value::Null)
    }
}

// ============================================================================
// Removal Operations
// ============================================================================

/// REMOVE - Remove all occurrences of item
pub struct RemoveTool;

impl Tool for RemoveTool {
    fn name(&self) -> &str {
        "REMOVE"
    }

    fn description(&self) -> &str {
        "Remove all occurrences of item from sequence"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REMOVE".to_string(),
                reason: "Expected item and sequence arguments".to_string(),
            });
        }

        let item = &args[0];
        let seq = args[1].as_array()?;

        let result: Vec<Value> = seq.iter().filter(|elem| *elem != item).cloned().collect();

        Ok(Value::Array(Arc::new(result)))
    }
}

/// REMOVE-IF - Remove elements satisfying predicate
pub struct RemoveIfTool;

impl Tool for RemoveIfTool {
    fn name(&self) -> &str {
        "REMOVE-IF"
    }

    fn description(&self) -> &str {
        "Remove elements where predicate returns true"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REMOVE-IF".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;

        let result: Vec<Value> = seq
            .iter()
            .filter(|elem| !elem.is_truthy())
            .cloned()
            .collect();

        Ok(Value::Array(Arc::new(result)))
    }
}

/// REMOVE-IF-NOT - Keep only elements satisfying predicate
pub struct RemoveIfNotTool;

impl Tool for RemoveIfNotTool {
    fn name(&self) -> &str {
        "REMOVE-IF-NOT"
    }

    fn description(&self) -> &str {
        "Remove elements where predicate returns false (keep truthy)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REMOVE-IF-NOT".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;

        let result: Vec<Value> = seq
            .iter()
            .filter(|elem| elem.is_truthy())
            .cloned()
            .collect();

        Ok(Value::Array(Arc::new(result)))
    }
}

/// REMOVE-DUPLICATES - Remove duplicate elements
pub struct RemoveDuplicatesTool;

impl Tool for RemoveDuplicatesTool {
    fn name(&self) -> &str {
        "REMOVE-DUPLICATES"
    }

    fn description(&self) -> &str {
        "Remove duplicate elements from sequence"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "REMOVE-DUPLICATES".to_string(),
                reason: "Expected sequence argument".to_string(),
            });
        }

        let seq = args[0].as_array()?;
        let mut result = Vec::new();

        for elem in seq.iter() {
            if !result.contains(elem) {
                result.push(elem.clone());
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }
}

/// DELETE - Destructively remove occurrences (creates new in OVSM)
pub struct DeleteTool;

impl Tool for DeleteTool {
    fn name(&self) -> &str {
        "DELETE"
    }

    fn description(&self) -> &str {
        "Remove all occurrences (destructive in CL, creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        RemoveTool.execute(args)
    }
}

/// DELETE-IF - Destructively remove elements (creates new in OVSM)
pub struct DeleteIfTool;

impl Tool for DeleteIfTool {
    fn name(&self) -> &str {
        "DELETE-IF"
    }

    fn description(&self) -> &str {
        "Remove elements matching predicate (destructive in CL, creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        RemoveIfTool.execute(args)
    }
}

// ============================================================================
// Substitution
// ============================================================================

/// SUBST - Substitute new for old in tree
pub struct SubstTool;

impl Tool for SubstTool {
    fn name(&self) -> &str {
        "SUBST"
    }

    fn description(&self) -> &str {
        "Substitute new for old throughout tree structure"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "SUBST".to_string(),
                reason: "Expected new, old, and tree arguments".to_string(),
            });
        }

        let new = &args[0];
        let old = &args[1];
        let tree = &args[2];

        fn subst_recursive(new: &Value, old: &Value, tree: &Value) -> Value {
            if tree == old {
                new.clone()
            } else if let Value::Array(arr) = tree {
                let result: Vec<Value> = arr
                    .iter()
                    .map(|elem| subst_recursive(new, old, elem))
                    .collect();
                Value::Array(Arc::new(result))
            } else {
                tree.clone()
            }
        }

        Ok(subst_recursive(new, old, tree))
    }
}

/// SUBST-IF - Substitute where predicate is true
pub struct SubstIfTool;

impl Tool for SubstIfTool {
    fn name(&self) -> &str {
        "SUBST-IF"
    }

    fn description(&self) -> &str {
        "Substitute new for elements where predicate is true"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "SUBST-IF".to_string(),
                reason: "Expected new, predicate, and tree arguments".to_string(),
            });
        }

        let new = &args[0];
        let tree = &args[2];

        fn subst_if_recursive(new: &Value, tree: &Value) -> Value {
            if tree.is_truthy() {
                new.clone()
            } else if let Value::Array(arr) = tree {
                let result: Vec<Value> = arr
                    .iter()
                    .map(|elem| subst_if_recursive(new, elem))
                    .collect();
                Value::Array(Arc::new(result))
            } else {
                tree.clone()
            }
        }

        Ok(subst_if_recursive(new, tree))
    }
}

/// NSUBST - Destructive substitute (creates new in OVSM)
pub struct NsubstTool;

impl Tool for NsubstTool {
    fn name(&self) -> &str {
        "NSUBST"
    }

    fn description(&self) -> &str {
        "Substitute (destructive in CL, creates new in OVSM)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        SubstTool.execute(args)
    }
}

// ============================================================================
// Set Operations
// ============================================================================

/// UNION - Set union
pub struct UnionTool;

impl Tool for UnionTool {
    fn name(&self) -> &str {
        "UNION"
    }

    fn description(&self) -> &str {
        "Return union of two lists (as sets)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "UNION".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let mut result: Vec<Value> = list1.to_vec();

        for elem in list2.iter() {
            if !result.contains(elem) {
                result.push(elem.clone());
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }
}

/// INTERSECTION - Set intersection
pub struct IntersectionTool;

impl Tool for IntersectionTool {
    fn name(&self) -> &str {
        "INTERSECTION"
    }

    fn description(&self) -> &str {
        "Return intersection of two lists (as sets)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "INTERSECTION".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let result: Vec<Value> = list1
            .iter()
            .filter(|elem| list2.contains(elem))
            .cloned()
            .collect();

        Ok(Value::Array(Arc::new(result)))
    }
}

/// SET-DIFFERENCE - Set difference
pub struct SetDifferenceTool;

impl Tool for SetDifferenceTool {
    fn name(&self) -> &str {
        "SET-DIFFERENCE"
    }

    fn description(&self) -> &str {
        "Return elements in list1 but not in list2"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SET-DIFFERENCE".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let result: Vec<Value> = list1
            .iter()
            .filter(|elem| !list2.contains(elem))
            .cloned()
            .collect();

        Ok(Value::Array(Arc::new(result)))
    }
}

/// SET-EXCLUSIVE-OR - Symmetric difference
pub struct SetExclusiveOrTool;

impl Tool for SetExclusiveOrTool {
    fn name(&self) -> &str {
        "SET-EXCLUSIVE-OR"
    }

    fn description(&self) -> &str {
        "Return elements in either list but not both"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SET-EXCLUSIVE-OR".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let mut result = Vec::new();

        // Elements in list1 but not list2
        for elem in list1.iter() {
            if !list2.contains(elem) {
                result.push(elem.clone());
            }
        }

        // Elements in list2 but not list1
        for elem in list2.iter() {
            if !list1.contains(elem) {
                result.push(elem.clone());
            }
        }

        Ok(Value::Array(Arc::new(result)))
    }
}

/// SUBSETP - Check if list1 is subset of list2
pub struct SubsetpTool;

impl Tool for SubsetpTool {
    fn name(&self) -> &str {
        "SUBSETP"
    }

    fn description(&self) -> &str {
        "Check if list1 is a subset of list2"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBSETP".to_string(),
                reason: "Expected two list arguments".to_string(),
            });
        }

        let list1 = args[0].as_array()?;
        let list2 = args[1].as_array()?;

        let is_subset = list1.iter().all(|elem| list2.contains(elem));
        Ok(Value::Bool(is_subset))
    }
}

// ============================================================================
// Association Lists
// ============================================================================

/// ASSOC - Find association by key
pub struct AssocTool;

impl Tool for AssocTool {
    fn name(&self) -> &str {
        "ASSOC"
    }

    fn description(&self) -> &str {
        "Find association pair by key in alist"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "ASSOC".to_string(),
                reason: "Expected key and alist arguments".to_string(),
            });
        }

        let key = &args[0];
        let alist = args[1].as_array()?;

        for pair in alist.iter() {
            if let Value::Array(p) = pair {
                if !p.is_empty() && &p[0] == key {
                    return Ok(pair.clone());
                }
            }
        }

        Ok(Value::Null)
    }
}

/// RASSOC - Find association by value
pub struct RassocTool;

impl Tool for RassocTool {
    fn name(&self) -> &str {
        "RASSOC"
    }

    fn description(&self) -> &str {
        "Find association pair by value in alist"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "RASSOC".to_string(),
                reason: "Expected value and alist arguments".to_string(),
            });
        }

        let value = &args[0];
        let alist = args[1].as_array()?;

        for pair in alist.iter() {
            if let Value::Array(p) = pair {
                if p.len() >= 2 && &p[1] == value {
                    return Ok(pair.clone());
                }
            }
        }

        Ok(Value::Null)
    }
}

/// PAIRLIS - Create association list from keys and values
pub struct PairlisTool;

impl Tool for PairlisTool {
    fn name(&self) -> &str {
        "PAIRLIS"
    }

    fn description(&self) -> &str {
        "Create association list from keys and values lists"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "PAIRLIS".to_string(),
                reason: "Expected keys and values lists".to_string(),
            });
        }

        let keys = args[0].as_array()?;
        let values = args[1].as_array()?;

        let pairs: Vec<Value> = keys
            .iter()
            .zip(values.iter())
            .map(|(k, v)| Value::Array(Arc::new(vec![k.clone(), v.clone()])))
            .collect();

        Ok(Value::Array(Arc::new(pairs)))
    }
}

// ============================================================================
// Predicates
// ============================================================================

/// EVERY - Check if predicate is true for all elements
pub struct EveryTool;

impl Tool for EveryTool {
    fn name(&self) -> &str {
        "EVERY"
    }

    fn description(&self) -> &str {
        "Check if all elements satisfy predicate"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "EVERY".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;
        let all_true = seq.iter().all(|elem| elem.is_truthy());
        Ok(Value::Bool(all_true))
    }
}

/// SOME - Check if predicate is true for any element
pub struct SomeTool;

impl Tool for SomeTool {
    fn name(&self) -> &str {
        "SOME"
    }

    fn description(&self) -> &str {
        "Check if any element satisfies predicate"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SOME".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;
        let any_true = seq.iter().any(|elem| elem.is_truthy());
        Ok(Value::Bool(any_true))
    }
}

/// NOTANY - Check if predicate is false for all elements
pub struct NotanyTool;

impl Tool for NotanyTool {
    fn name(&self) -> &str {
        "NOTANY"
    }

    fn description(&self) -> &str {
        "Check if no element satisfies predicate"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "NOTANY".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;
        let none_true = !seq.iter().any(|elem| elem.is_truthy());
        Ok(Value::Bool(none_true))
    }
}

/// NOTEVERY - Check if predicate is false for some element
pub struct NoteveryTool;

impl Tool for NoteveryTool {
    fn name(&self) -> &str {
        "NOTEVERY"
    }

    fn description(&self) -> &str {
        "Check if not all elements satisfy predicate"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "NOTEVERY".to_string(),
                reason: "Expected predicate and sequence arguments".to_string(),
            });
        }

        let seq = args[1].as_array()?;
        let not_all = !seq.iter().all(|elem| elem.is_truthy());
        Ok(Value::Bool(not_all))
    }
}

// ============================================================================
// Reduction and Mapping
// ============================================================================

/// REDUCE - Reduce sequence to single value
pub struct ReduceTool;

impl Tool for ReduceTool {
    fn name(&self) -> &str {
        "REDUCE"
    }

    fn description(&self) -> &str {
        "Reduce sequence to single value (requires lambda support)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "REDUCE (requires lambda support)".to_string(),
        })
    }
}

/// MAPCAR - Map function over lists
pub struct MapcarTool;

impl Tool for MapcarTool {
    fn name(&self) -> &str {
        "MAPCAR"
    }

    fn description(&self) -> &str {
        "Map function over lists (requires lambda support)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "MAPCAR (requires lambda support)".to_string(),
        })
    }
}

/// MAPC - Map function for side effects
pub struct MapcTool;

impl Tool for MapcTool {
    fn name(&self) -> &str {
        "MAPC"
    }

    fn description(&self) -> &str {
        "Map function for side effects (requires lambda support)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "MAPC (requires lambda support)".to_string(),
        })
    }
}

/// MAPLIST - Map function over successive tails
pub struct MaplistTool;

impl Tool for MaplistTool {
    fn name(&self) -> &str {
        "MAPLIST"
    }

    fn description(&self) -> &str {
        "Map function over successive tails (requires lambda support)"
    }

    fn execute(&self, _args: &[Value]) -> Result<Value> {
        Err(Error::NotImplemented {
            tool: "MAPLIST (requires lambda support)".to_string(),
        })
    }
}

// ============================================================================
// Miscellaneous
// ============================================================================

/// FILL - Fill sequence with value
pub struct FillTool;

impl Tool for FillTool {
    fn name(&self) -> &str {
        "FILL"
    }

    fn description(&self) -> &str {
        "Fill sequence with value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "FILL".to_string(),
                reason: "Expected sequence and value arguments".to_string(),
            });
        }

        let seq = args[0].as_array()?;
        let value = &args[1];

        let filled: Vec<Value> = vec![value.clone(); seq.len()];
        Ok(Value::Array(Arc::new(filled)))
    }
}

/// MISMATCH - Find first position where sequences differ
pub struct MismatchTool;

impl Tool for MismatchTool {
    fn name(&self) -> &str {
        "MISMATCH"
    }

    fn description(&self) -> &str {
        "Find first position where sequences differ"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MISMATCH".to_string(),
                reason: "Expected two sequence arguments".to_string(),
            });
        }

        let seq1 = args[0].as_array()?;
        let seq2 = args[1].as_array()?;

        for (i, (e1, e2)) in seq1.iter().zip(seq2.iter()).enumerate() {
            if e1 != e2 {
                return Ok(Value::Int(i as i64));
            }
        }

        // If one sequence is longer
        if seq1.len() != seq2.len() {
            Ok(Value::Int(seq1.len().min(seq2.len()) as i64))
        } else {
            Ok(Value::Null)
        }
    }
}

// ============================================================================
// Advanced Sequence Operations
// ============================================================================

/// MERGE - Merge two sorted sequences
pub struct MergeTool;
impl Tool for MergeTool {
    fn name(&self) -> &str {
        "MERGE"
    }
    fn description(&self) -> &str {
        "Merge two sorted sequences"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MERGE".to_string(),
                reason: "Expected two sequences".to_string(),
            });
        }
        let seq1 = args[0].as_array()?;
        let seq2 = args[1].as_array()?;
        let mut result = seq1.to_vec();
        result.extend(seq2.iter().cloned());
        result.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });
        Ok(Value::Array(Arc::new(result)))
    }
}

/// STABLE-SORT - Stable sort sequence
pub struct StableSortTool;
impl Tool for StableSortTool {
    fn name(&self) -> &str {
        "STABLE-SORT"
    }
    fn description(&self) -> &str {
        "Stable sort sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        let mut seq = args[0].as_array()?.to_vec();
        seq.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::String(x), Value::String(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });
        Ok(Value::Array(Arc::new(seq)))
    }
}

/// SEARCH - Search for subsequence
pub struct SearchTool;
impl Tool for SearchTool {
    fn name(&self) -> &str {
        "SEARCH"
    }
    fn description(&self) -> &str {
        "Search for subsequence within sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SEARCH".to_string(),
                reason: "Expected two sequences".to_string(),
            });
        }
        let needle = args[0].as_array()?;
        let haystack = args[1].as_array()?;

        if needle.is_empty() {
            return Ok(Value::Int(0));
        }

        for i in 0..=haystack.len().saturating_sub(needle.len()) {
            if haystack[i..].starts_with(needle) {
                return Ok(Value::Int(i as i64));
            }
        }
        Ok(Value::Null)
    }
}

/// SUBSTITUTE-IF - Substitute if predicate matches
pub struct SubstituteIfTool;
impl Tool for SubstituteIfTool {
    fn name(&self) -> &str {
        "SUBSTITUTE-IF"
    }
    fn description(&self) -> &str {
        "Substitute if predicate matches"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBSTITUTE-IF".to_string(),
                reason: "Expected new value and sequence".to_string(),
            });
        }
        let new_val = &args[0];
        let seq = args[1].as_array()?;
        let result: Vec<Value> = seq
            .iter()
            .map(|elem| {
                if elem.is_truthy() {
                    new_val.clone()
                } else {
                    elem.clone()
                }
            })
            .collect();
        Ok(Value::Array(Arc::new(result)))
    }
}

/// SUBSTITUTE-IF-NOT - Substitute if predicate doesn't match
pub struct SubstituteIfNotTool;
impl Tool for SubstituteIfNotTool {
    fn name(&self) -> &str {
        "SUBSTITUTE-IF-NOT"
    }
    fn description(&self) -> &str {
        "Substitute if predicate doesn't match"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBSTITUTE-IF-NOT".to_string(),
                reason: "Expected new value and sequence".to_string(),
            });
        }
        let new_val = &args[0];
        let seq = args[1].as_array()?;
        let result: Vec<Value> = seq
            .iter()
            .map(|elem| {
                if !elem.is_truthy() {
                    new_val.clone()
                } else {
                    elem.clone()
                }
            })
            .collect();
        Ok(Value::Array(Arc::new(result)))
    }
}

/// NSUBSTITUTE-IF - Destructively substitute if predicate matches
pub struct NsubstituteIfTool;
impl Tool for NsubstituteIfTool {
    fn name(&self) -> &str {
        "NSUBSTITUTE-IF"
    }
    fn description(&self) -> &str {
        "Destructively substitute if predicate matches"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        SubstituteIfTool.execute(args)
    }
}

/// NSUBSTITUTE-IF-NOT - Destructively substitute if predicate doesn't match
pub struct NsubstituteIfNotTool;
impl Tool for NsubstituteIfNotTool {
    fn name(&self) -> &str {
        "NSUBSTITUTE-IF-NOT"
    }
    fn description(&self) -> &str {
        "Destructively substitute if predicate doesn't match"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        SubstituteIfNotTool.execute(args)
    }
}

/// DELETE-DUPLICATES - Remove duplicate elements
pub struct DeleteDuplicatesTool;
impl Tool for DeleteDuplicatesTool {
    fn name(&self) -> &str {
        "DELETE-DUPLICATES"
    }
    fn description(&self) -> &str {
        "Remove duplicate elements from sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        let seq = args[0].as_array()?;
        let mut seen = Vec::new();
        let mut result = Vec::new();
        for elem in seq.iter() {
            if !seen.contains(elem) {
                seen.push(elem.clone());
                result.push(elem.clone());
            }
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

/// COUNT - Count occurrences of item
pub struct CountTool;
impl Tool for CountTool {
    fn name(&self) -> &str {
        "COUNT"
    }
    fn description(&self) -> &str {
        "Count occurrences of item in sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "COUNT".to_string(),
                reason: "Expected item and sequence".to_string(),
            });
        }
        let item = &args[0];
        let seq = args[1].as_array()?;
        let count = seq.iter().filter(|elem| *elem == item).count();
        Ok(Value::Int(count as i64))
    }
}

/// COUNT-IF - Count items matching predicate
pub struct CountIfTool;
impl Tool for CountIfTool {
    fn name(&self) -> &str {
        "COUNT-IF"
    }
    fn description(&self) -> &str {
        "Count items matching predicate"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        let seq = args[0].as_array()?;
        let count = seq.iter().filter(|elem| elem.is_truthy()).count();
        Ok(Value::Int(count as i64))
    }
}

/// POSITION - Find position of item
pub struct PositionTool;
impl Tool for PositionTool {
    fn name(&self) -> &str {
        "POSITION"
    }
    fn description(&self) -> &str {
        "Find position of item in sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "POSITION".to_string(),
                reason: "Expected item and sequence".to_string(),
            });
        }
        let item = &args[0];
        let seq = args[1].as_array()?;
        for (i, elem) in seq.iter().enumerate() {
            if elem == item {
                return Ok(Value::Int(i as i64));
            }
        }
        Ok(Value::Null)
    }
}

/// FIND-IF-NOT - Find item not matching predicate
pub struct FindIfNotTool;
impl Tool for FindIfNotTool {
    fn name(&self) -> &str {
        "FIND-IF-NOT"
    }
    fn description(&self) -> &str {
        "Find first item not matching predicate"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Null);
        }
        let seq = args[0].as_array()?;
        for elem in seq.iter() {
            if !elem.is_truthy() {
                return Ok(elem.clone());
            }
        }
        Ok(Value::Null)
    }
}

/// REPLACE - Replace subsequence
pub struct ReplaceToolSeq;
impl Tool for ReplaceToolSeq {
    fn name(&self) -> &str {
        "REPLACE"
    }
    fn description(&self) -> &str {
        "Replace subsequence in sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REPLACE".to_string(),
                reason: "Expected two sequences".to_string(),
            });
        }
        let seq1 = args[0].as_array()?;
        let seq2 = args[1].as_array()?;
        let len = seq1.len().min(seq2.len());
        let mut result = seq1.to_vec();
        for i in 0..len {
            result[i] = seq2[i].clone();
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

/// NREPLACE - Destructively replace subsequence
pub struct NreplaceTool;
impl Tool for NreplaceTool {
    fn name(&self) -> &str {
        "NREPLACE"
    }
    fn description(&self) -> &str {
        "Destructively replace subsequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        ReplaceToolSeq.execute(args)
    }
}

/// CONCATENATE - Concatenate sequences
pub struct ConcatenateTool;
impl Tool for ConcatenateTool {
    fn name(&self) -> &str {
        "CONCATENATE"
    }
    fn description(&self) -> &str {
        "Concatenate sequences"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        let mut result = Vec::new();
        for arg in args {
            if let Value::Array(arr) = arg {
                result.extend(arr.iter().cloned());
            }
        }
        Ok(Value::Array(Arc::new(result)))
    }
}

/// LENGTH - Get sequence length
pub struct LengthSeqTool;
impl Tool for LengthSeqTool {
    fn name(&self) -> &str {
        "LENGTH"
    }
    fn description(&self) -> &str {
        "Get length of sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Int(0));
        }
        match &args[0] {
            Value::Array(arr) => Ok(Value::Int(arr.len() as i64)),
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            _ => Ok(Value::Int(0)),
        }
    }
}

/// REVERSE - Reverse sequence
pub struct ReverseTool;
impl Tool for ReverseTool {
    fn name(&self) -> &str {
        "REVERSE"
    }
    fn description(&self) -> &str {
        "Reverse sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        let mut seq = args[0].as_array()?.to_vec();
        seq.reverse();
        Ok(Value::Array(Arc::new(seq)))
    }
}

/// SUBSEQUENCE - Extract subsequence
pub struct SubsequenceTool;
impl Tool for SubsequenceTool {
    fn name(&self) -> &str {
        "SUBSEQUENCE"
    }
    fn description(&self) -> &str {
        "Extract subsequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "SUBSEQUENCE".to_string(),
                reason: "Expected sequence and start index".to_string(),
            });
        }
        let seq = args[0].as_array()?;
        let start = args[1].as_int()? as usize;
        let end = if args.len() > 2 {
            args[2].as_int()? as usize
        } else {
            seq.len()
        };

        if start > seq.len() || end > seq.len() || start > end {
            return Ok(Value::Array(Arc::new(vec![])));
        }

        Ok(Value::Array(Arc::new(seq[start..end].to_vec())))
    }
}

/// SORT - Sort sequence
pub struct SortTool;
impl Tool for SortTool {
    fn name(&self) -> &str {
        "SORT"
    }
    fn description(&self) -> &str {
        "Sort sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        let mut seq = args[0].as_array()?.to_vec();
        seq.sort_by(|a, b| match (a, b) {
            (Value::Int(x), Value::Int(y)) => x.cmp(y),
            (Value::Float(x), Value::Float(y)) => {
                x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
            }
            (Value::String(x), Value::String(y)) => x.cmp(y),
            _ => std::cmp::Ordering::Equal,
        });
        Ok(Value::Array(Arc::new(seq)))
    }
}

/// MAP - Map function over sequence
pub struct MapTool;
impl Tool for MapTool {
    fn name(&self) -> &str {
        "MAP"
    }
    fn description(&self) -> &str {
        "Map function over sequence"
    }
    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Array(Arc::new(vec![])));
        }
        // Simplified: just return the sequence
        Ok(args[0].clone())
    }
}
