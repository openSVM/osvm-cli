//! Struct type definitions for compile-time layout

use std::collections::HashMap;

/// Primitive field types (fixed-size scalars)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    U8,  // 1 byte
    U16, // 2 bytes
    U32, // 4 bytes
    U64, // 8 bytes (default for untyped)
    I8,  // 1 byte signed
    I16, // 2 bytes signed
    I32, // 4 bytes signed
    I64, // 8 bytes signed (default)
}

impl PrimitiveType {
    pub fn size(&self) -> i64 {
        match self {
            PrimitiveType::U8 | PrimitiveType::I8 => 1,
            PrimitiveType::U16 | PrimitiveType::I16 => 2,
            PrimitiveType::U32 | PrimitiveType::I32 => 4,
            PrimitiveType::U64 | PrimitiveType::I64 => 8,
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s {
            "u8" => Some(PrimitiveType::U8),
            "u16" => Some(PrimitiveType::U16),
            "u32" => Some(PrimitiveType::U32),
            "u64" => Some(PrimitiveType::U64),
            "i8" => Some(PrimitiveType::I8),
            "i16" => Some(PrimitiveType::I16),
            "i32" => Some(PrimitiveType::I32),
            "i64" => Some(PrimitiveType::I64),
            _ => None,
        }
    }

    pub fn to_idl_type(&self) -> &'static str {
        match self {
            PrimitiveType::U8 => "u8",
            PrimitiveType::U16 => "u16",
            PrimitiveType::U32 => "u32",
            PrimitiveType::U64 => "u64",
            PrimitiveType::I8 => "i8",
            PrimitiveType::I16 => "i16",
            PrimitiveType::I32 => "i32",
            PrimitiveType::I64 => "i64",
        }
    }
}

/// Extended field type supporting primitives, arrays, pubkeys, and nested structs
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FieldType {
    /// Primitive integer types (u8-u64, i8-i64)
    Primitive(PrimitiveType),
    /// Fixed-size array: [element_type count], e.g., [u32 10] = 40 bytes
    Array {
        element_type: PrimitiveType,
        count: usize,
    },
    /// Solana public key (32 bytes, special handling)
    Pubkey,
    /// Nested struct reference (resolved at struct definition time)
    Struct(String),
}

impl FieldType {
    /// Get size in bytes (for Array and Struct, needs struct_defs for resolution)
    pub fn size(&self) -> i64 {
        match self {
            FieldType::Primitive(p) => p.size(),
            FieldType::Array {
                element_type,
                count,
            } => element_type.size() * (*count as i64),
            FieldType::Pubkey => 32,   // Solana pubkey is always 32 bytes
            FieldType::Struct(_) => 0, // Requires struct_defs lookup - use size_with_structs
        }
    }

    /// Get size with struct definitions for nested struct resolution
    pub fn size_with_structs(&self, struct_defs: &HashMap<String, StructDef>) -> i64 {
        match self {
            FieldType::Struct(name) => struct_defs.get(name).map(|s| s.total_size).unwrap_or(0),
            _ => self.size(),
        }
    }

    /// Parse from type string (simple types only - arrays/structs handled separately)
    pub fn parse(s: &str) -> Option<Self> {
        if s == "pubkey" {
            return Some(FieldType::Pubkey);
        }
        PrimitiveType::parse(s).map(FieldType::Primitive)
    }

    /// Convert to Anchor IDL type string
    pub fn to_idl_type(&self) -> String {
        match self {
            FieldType::Primitive(p) => p.to_idl_type().to_string(),
            FieldType::Array {
                element_type,
                count,
            } => {
                format!(
                    "{{ \"array\": [\"{}\", {}] }}",
                    element_type.to_idl_type(),
                    count
                )
            }
            FieldType::Pubkey => "publicKey".to_string(),
            FieldType::Struct(name) => format!("{{ \"defined\": \"{}\" }}", name),
        }
    }

    /// Check if this is a primitive type for load/store instruction selection
    pub fn primitive(&self) -> Option<PrimitiveType> {
        match self {
            FieldType::Primitive(p) => Some(*p),
            _ => None,
        }
    }
}

/// A field in a struct definition
#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub field_type: FieldType,
    pub offset: i64,
    /// For array types, the element size
    pub element_size: Option<i64>,
    /// For array types, the element count
    pub array_count: Option<usize>,
}

/// A struct definition (compile-time metadata)
#[derive(Debug, Clone)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub total_size: i64,
}

impl StructDef {
    /// Generate Anchor IDL JSON for this struct
    /// This enables TypeScript clients to interact with OVSM programs
    pub fn to_anchor_idl(&self) -> String {
        let mut fields_json = Vec::new();
        for field in &self.fields {
            fields_json.push(format!(
                r#"        {{ "name": "{}", "type": "{}" }}"#,
                field.name,
                field.field_type.to_idl_type()
            ));
        }

        format!(
            r#"{{
  "name": "{}",
  "type": {{
    "kind": "struct",
    "fields": [
{}
    ]
  }}
}}"#,
            self.name,
            fields_json.join(",\n")
        )
    }
}
