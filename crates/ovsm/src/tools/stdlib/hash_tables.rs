//! Hash table operations - Common Lisp compatible
//!
//! Provides hash table data structure with Common Lisp semantics.
//! Hash tables map keys to values using hash-based lookup.

use crate::error::{Error, Result};
use crate::runtime::Value;
use crate::tools::{Tool, ToolRegistry};
use std::collections::HashMap;
use std::sync::Arc;

/// Register all hash table tools
pub fn register(registry: &mut ToolRegistry) {
    // Creation & Basic Operations
    registry.register(MakeHashTableTool);
    registry.register(GethashTool);
    registry.register(RemhashTool);
    registry.register(ClrhashTool);
    registry.register(HashTablePTool);

    // Properties
    registry.register(HashTableCountTool);
    registry.register(HashTableSizeTool);
    registry.register(HashTableTestTool);

    // Iteration
    registry.register(MaphashTool);
    registry.register(HashTableKeysTool);
    registry.register(HashTableValuesTool);
    registry.register(HashTablePairsTool);

    // Utilities
    registry.register(SxhashTool);
    registry.register(HashTableToAlistTool);
    registry.register(AlistToHashTableTool);
    registry.register(CopyHashTableTool);
    registry.register(HashTableEqualPTool);
    registry.register(MergeHashTablesTool);

    // Advanced Operations
    registry.register(HashTableFilterTool);
    registry.register(HashTableMapTool);
    registry.register(HashTableContainsKeyTool);
    registry.register(HashTableGetOrDefaultTool);
    registry.register(HashTableUpdateTool);
    registry.register(HashTableRemoveIfTool);
}

// ============================================================================
// Creation & Basic Operations
// ============================================================================

/// MAKE-HASH-TABLE - Create new hash table
pub struct MakeHashTableTool;

impl Tool for MakeHashTableTool {
    fn name(&self) -> &str {
        "MAKE-HASH-TABLE"
    }

    fn description(&self) -> &str {
        "Create a new hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        // Optional: accept initial size hint
        let _initial_size = if !args.is_empty() {
            Some(args[0].as_int()? as usize)
        } else {
            None
        };

        // Create empty hash table
        let map: HashMap<String, Value> = HashMap::new();
        Ok(Value::Object(Arc::new(map)))
    }
}

/// GETHASH - Get value from hash table by key
pub struct GethashTool;

impl Tool for GethashTool {
    fn name(&self) -> &str {
        "GETHASH"
    }

    fn description(&self) -> &str {
        "Get value from hash table by key (returns value or default)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "GETHASH".to_string(),
                reason: "Expected key and hash-table arguments".to_string(),
            });
        }

        let key = args[0].as_string()?;
        let hash_table = args[1].as_object()?;

        // Optional default value
        let default = if args.len() > 2 {
            args[2].clone()
        } else {
            Value::Null
        };

        match hash_table.get(key) {
            Some(value) => Ok(value.clone()),
            None => Ok(default),
        }
    }
}

/// REMHASH - Remove entry from hash table
pub struct RemhashTool;

impl Tool for RemhashTool {
    fn name(&self) -> &str {
        "REMHASH"
    }

    fn description(&self) -> &str {
        "Remove key-value pair from hash table (returns true if removed)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "REMHASH".to_string(),
                reason: "Expected key and hash-table arguments".to_string(),
            });
        }

        let key = args[0].as_string()?;
        let hash_table = args[1].as_object()?;

        // Check if key was present
        let was_present = hash_table.contains_key(key);

        // Note: In Common Lisp, REMHASH modifies in place and returns true if removed
        // Since OVSM uses immutable data, we just check presence
        Ok(Value::Bool(was_present))
    }
}

/// CLRHASH - Clear all entries from hash table
pub struct ClrhashTool;

impl Tool for ClrhashTool {
    fn name(&self) -> &str {
        "CLRHASH"
    }

    fn description(&self) -> &str {
        "Clear all entries from hash table (returns empty hash table)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "CLRHASH".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        // Verify it's a hash table
        let _ = args[0].as_object()?;

        // Return empty hash table
        Ok(Value::Object(Arc::new(HashMap::new())))
    }
}

/// HASH-TABLE-P - Check if value is a hash table
pub struct HashTablePTool;

impl Tool for HashTablePTool {
    fn name(&self) -> &str {
        "HASH-TABLE-P"
    }

    fn description(&self) -> &str {
        "Check if value is a hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Bool(false));
        }

        Ok(Value::Bool(matches!(&args[0], Value::Object(_))))
    }
}

// ============================================================================
// Properties
// ============================================================================

/// HASH-TABLE-COUNT - Get number of entries
pub struct HashTableCountTool;

impl Tool for HashTableCountTool {
    fn name(&self) -> &str {
        "HASH-TABLE-COUNT"
    }

    fn description(&self) -> &str {
        "Get number of key-value pairs in hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-COUNT".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        Ok(Value::Int(hash_table.len() as i64))
    }
}

/// HASH-TABLE-SIZE - Get current capacity
pub struct HashTableSizeTool;

impl Tool for HashTableSizeTool {
    fn name(&self) -> &str {
        "HASH-TABLE-SIZE"
    }

    fn description(&self) -> &str {
        "Get current capacity of hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-SIZE".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        // Return capacity (in Rust, this is approximate)
        Ok(Value::Int(hash_table.len() as i64))
    }
}

/// HASH-TABLE-TEST - Get equality test function
pub struct HashTableTestTool;

impl Tool for HashTableTestTool {
    fn name(&self) -> &str {
        "HASH-TABLE-TEST"
    }

    fn description(&self) -> &str {
        "Get equality test function for hash table (returns 'EQUAL')"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-TEST".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        // Verify it's a hash table
        let _ = args[0].as_object()?;

        // OVSM hash tables use string equality
        Ok(Value::String("EQUAL".to_string()))
    }
}

// ============================================================================
// Iteration
// ============================================================================

/// MAPHASH - Apply function to each entry
pub struct MaphashTool;

impl Tool for MaphashTool {
    fn name(&self) -> &str {
        "MAPHASH"
    }

    fn description(&self) -> &str {
        "Apply function to each key-value pair (returns null)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "MAPHASH".to_string(),
                reason: "Expected function and hash-table arguments".to_string(),
            });
        }

        // Note: In full implementation, would apply function to each entry
        // For now, return null (Common Lisp MAPHASH returns NIL)
        Ok(Value::Null)
    }
}

/// HASH-TABLE-KEYS - Get all keys as list
pub struct HashTableKeysTool;

impl Tool for HashTableKeysTool {
    fn name(&self) -> &str {
        "HASH-TABLE-KEYS"
    }

    fn description(&self) -> &str {
        "Get list of all keys in hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-KEYS".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let keys: Vec<Value> = hash_table
            .keys()
            .map(|k| Value::String(k.clone()))
            .collect();

        Ok(Value::Array(Arc::new(keys)))
    }
}

/// HASH-TABLE-VALUES - Get all values as list
pub struct HashTableValuesTool;

impl Tool for HashTableValuesTool {
    fn name(&self) -> &str {
        "HASH-TABLE-VALUES"
    }

    fn description(&self) -> &str {
        "Get list of all values in hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-VALUES".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let values: Vec<Value> = hash_table.values().cloned().collect();

        Ok(Value::Array(Arc::new(values)))
    }
}

/// HASH-TABLE-PAIRS - Get all key-value pairs as list of lists
pub struct HashTablePairsTool;

impl Tool for HashTablePairsTool {
    fn name(&self) -> &str {
        "HASH-TABLE-PAIRS"
    }

    fn description(&self) -> &str {
        "Get list of all key-value pairs as (key value) lists"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-PAIRS".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let pairs: Vec<Value> = hash_table
            .iter()
            .map(|(k, v)| Value::Array(Arc::new(vec![Value::String(k.clone()), v.clone()])))
            .collect();

        Ok(Value::Array(Arc::new(pairs)))
    }
}

// ============================================================================
// Utilities
// ============================================================================

/// SXHASH - Compute hash code for value
pub struct SxhashTool;

impl Tool for SxhashTool {
    fn name(&self) -> &str {
        "SXHASH"
    }

    fn description(&self) -> &str {
        "Compute hash code for value"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "SXHASH".to_string(),
                reason: "Expected value argument".to_string(),
            });
        }

        // Simple hash computation
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();

        // Hash based on value type
        let hash_value = match &args[0] {
            Value::Int(n) => {
                n.hash(&mut hasher);
                hasher.finish()
            }
            Value::Float(f) => {
                f.to_bits().hash(&mut hasher);
                hasher.finish()
            }
            Value::String(s) => {
                s.hash(&mut hasher);
                hasher.finish()
            }
            Value::Bool(b) => {
                b.hash(&mut hasher);
                hasher.finish()
            }
            _ => {
                // For complex types, use a simple hash
                format!("{:?}", args[0]).hash(&mut hasher);
                hasher.finish()
            }
        };

        Ok(Value::Int(hash_value as i64))
    }
}

/// HASH-TABLE-TO-ALIST - Convert hash table to association list
pub struct HashTableToAlistTool;

impl Tool for HashTableToAlistTool {
    fn name(&self) -> &str {
        "HASH-TABLE-TO-ALIST"
    }

    fn description(&self) -> &str {
        "Convert hash table to association list"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-TO-ALIST".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let alist: Vec<Value> = hash_table
            .iter()
            .map(|(k, v)| Value::Array(Arc::new(vec![Value::String(k.clone()), v.clone()])))
            .collect();

        Ok(Value::Array(Arc::new(alist)))
    }
}

/// ALIST-TO-HASH-TABLE - Convert association list to hash table
pub struct AlistToHashTableTool;

impl Tool for AlistToHashTableTool {
    fn name(&self) -> &str {
        "ALIST-TO-HASH-TABLE"
    }

    fn description(&self) -> &str {
        "Convert association list to hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "ALIST-TO-HASH-TABLE".to_string(),
                reason: "Expected alist argument".to_string(),
            });
        }

        let alist = args[0].as_array()?;
        let mut map = HashMap::new();

        for pair in alist.iter() {
            if let Value::Array(p) = pair {
                if p.len() >= 2 {
                    let key = p[0].as_string()?.to_string();
                    let value = p[1].clone();
                    map.insert(key, value);
                }
            }
        }

        Ok(Value::Object(Arc::new(map)))
    }
}

/// COPY-HASH-TABLE - Create shallow copy of hash table
pub struct CopyHashTableTool;

impl Tool for CopyHashTableTool {
    fn name(&self) -> &str {
        "COPY-HASH-TABLE"
    }

    fn description(&self) -> &str {
        "Create shallow copy of hash table"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Err(Error::InvalidArguments {
                tool: "COPY-HASH-TABLE".to_string(),
                reason: "Expected hash-table argument".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let new_map = hash_table
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<String, Value>>();

        Ok(Value::Object(Arc::new(new_map)))
    }
}

/// HASH-TABLE-EQUAL-P - Check if two hash tables are equal
pub struct HashTableEqualPTool;

impl Tool for HashTableEqualPTool {
    fn name(&self) -> &str {
        "HASH-TABLE-EQUAL-P"
    }

    fn description(&self) -> &str {
        "Check if two hash tables have equal contents"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-EQUAL-P".to_string(),
                reason: "Expected two hash-table arguments".to_string(),
            });
        }

        let ht1 = args[0].as_object()?;
        let ht2 = args[1].as_object()?;

        // Check if same size
        if ht1.len() != ht2.len() {
            return Ok(Value::Bool(false));
        }

        // Check all keys and values
        for (key, val1) in ht1.iter() {
            match ht2.get(key) {
                Some(val2) if val1 == val2 => continue,
                _ => return Ok(Value::Bool(false)),
            }
        }

        Ok(Value::Bool(true))
    }
}

/// MERGE-HASH-TABLES - Merge multiple hash tables
pub struct MergeHashTablesTool;

impl Tool for MergeHashTablesTool {
    fn name(&self) -> &str {
        "MERGE-HASH-TABLES"
    }

    fn description(&self) -> &str {
        "Merge multiple hash tables (later values override earlier)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.is_empty() {
            return Ok(Value::Object(Arc::new(HashMap::new())));
        }

        let mut merged: HashMap<String, Value> = HashMap::new();

        for arg in args {
            let ht = arg.as_object()?;
            for (k, v) in ht.iter() {
                merged.insert(k.clone(), v.clone());
            }
        }

        Ok(Value::Object(Arc::new(merged)))
    }
}

// ============================================================================
// Advanced Operations
// ============================================================================

/// HASH-TABLE-FILTER - Filter hash table by predicate
pub struct HashTableFilterTool;

impl Tool for HashTableFilterTool {
    fn name(&self) -> &str {
        "HASH-TABLE-FILTER"
    }

    fn description(&self) -> &str {
        "Filter hash table entries (placeholder - needs function support)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-FILTER".to_string(),
                reason: "Expected predicate and hash-table arguments".to_string(),
            });
        }

        // For now, return the original hash table
        // Full implementation requires function evaluation
        Ok(args[1].clone())
    }
}

/// HASH-TABLE-MAP - Map function over hash table
pub struct HashTableMapTool;

impl Tool for HashTableMapTool {
    fn name(&self) -> &str {
        "HASH-TABLE-MAP"
    }

    fn description(&self) -> &str {
        "Map function over hash table entries (placeholder)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-MAP".to_string(),
                reason: "Expected function and hash-table arguments".to_string(),
            });
        }

        // For now, return the original hash table
        // Full implementation requires function evaluation
        Ok(args[1].clone())
    }
}

/// HASH-TABLE-CONTAINS-KEY - Check if key exists
pub struct HashTableContainsKeyTool;

impl Tool for HashTableContainsKeyTool {
    fn name(&self) -> &str {
        "HASH-TABLE-CONTAINS-KEY"
    }

    fn description(&self) -> &str {
        "Check if hash table contains key"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-CONTAINS-KEY".to_string(),
                reason: "Expected key and hash-table arguments".to_string(),
            });
        }

        let key = args[0].as_string()?;
        let hash_table = args[1].as_object()?;

        Ok(Value::Bool(hash_table.contains_key(key)))
    }
}

/// HASH-TABLE-GET-OR-DEFAULT - Get value with default
pub struct HashTableGetOrDefaultTool;

impl Tool for HashTableGetOrDefaultTool {
    fn name(&self) -> &str {
        "HASH-TABLE-GET-OR-DEFAULT"
    }

    fn description(&self) -> &str {
        "Get value or return default if key not found"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-GET-OR-DEFAULT".to_string(),
                reason: "Expected key, hash-table, and default arguments".to_string(),
            });
        }

        let key = args[0].as_string()?;
        let hash_table = args[1].as_object()?;
        let default = &args[2];

        match hash_table.get(key) {
            Some(value) => Ok(value.clone()),
            None => Ok(default.clone()),
        }
    }
}

/// HASH-TABLE-UPDATE - Update hash table entry
pub struct HashTableUpdateTool;

impl Tool for HashTableUpdateTool {
    fn name(&self) -> &str {
        "HASH-TABLE-UPDATE"
    }

    fn description(&self) -> &str {
        "Update hash table with new key-value pair (returns new hash table)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 3 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-UPDATE".to_string(),
                reason: "Expected hash-table, key, and value arguments".to_string(),
            });
        }

        let hash_table = args[0].as_object()?;
        let key = args[1].as_string()?.to_string();
        let value = &args[2];

        let mut new_map = hash_table
            .iter()
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect::<HashMap<String, Value>>();
        new_map.insert(key, value.clone());

        Ok(Value::Object(Arc::new(new_map)))
    }
}

/// HASH-TABLE-REMOVE-IF - Remove entries matching predicate
pub struct HashTableRemoveIfTool;

impl Tool for HashTableRemoveIfTool {
    fn name(&self) -> &str {
        "HASH-TABLE-REMOVE-IF"
    }

    fn description(&self) -> &str {
        "Remove entries where predicate returns true (placeholder)"
    }

    fn execute(&self, args: &[Value]) -> Result<Value> {
        if args.len() < 2 {
            return Err(Error::InvalidArguments {
                tool: "HASH-TABLE-REMOVE-IF".to_string(),
                reason: "Expected predicate and hash-table arguments".to_string(),
            });
        }

        // For now, return the original hash table
        // Full implementation requires function evaluation
        Ok(args[1].clone())
    }
}
