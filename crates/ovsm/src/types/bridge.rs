//! # Type Bridge: Connecting Source Types to Memory Model
//!
//! This module provides the critical bridge between:
//! - **Source Types** (`types::Type`): The user-facing type system from bidirectional inference
//! - **IR Types** (`compiler::ir::RegType`): The low-level memory model types
//!
//! ## Why This Bridge Exists
//!
//! The OVSM compiler has two separate type systems:
//!
//! 1. **Source Level** (`Type`): Captures programmer intent
//!    - `u64`, `i32`, `Pubkey`, `[u8; 32]`, `fn(i64) -> bool`
//!    - Supports gradual typing via `Any`
//!    - Used by the bidirectional checker for type inference
//!
//! 2. **IR Level** (`RegType`): Captures memory layout
//!    - `Value { size: 8, signed: false }` vs `Pointer(...)`
//!    - Tracks memory regions, bounds, alignment
//!    - Used for memory safety validation
//!
//! The bridge translates between these, ensuring:
//! - Type annotations constrain memory operations
//! - Memory model gets full provenance information from source types
//! - Errors caught at either level are unified
//!
//! ## Usage
//!
//! ```rust,ignore
//! use ovsm::types::bridge::TypeBridge;
//!
//! let mut bridge = TypeBridge::new();
//!
//! // Source type annotation informs memory model
//! let source_type = Type::Ptr(Box::new(Type::Struct("MyStruct".into())));
//! let reg_type = bridge.source_to_ir(&source_type, &type_context);
//!
//! // Now reg_type contains pointer provenance with struct info
//! ```

use super::{Type, TypeContext, TypedStructDef};
use crate::compiler::ir::{
    Alignment, FieldType, MemoryRegion, PointerType, PrimitiveType, RegType, StructDef,
};
use std::collections::HashMap;

/// Bridge between source-level types and IR-level types
pub struct TypeBridge {
    /// Cache of converted struct definitions
    struct_cache: HashMap<String, StructDef>,
    /// Default memory region for pointers (can be overridden)
    default_region: MemoryRegion,
}

impl TypeBridge {
    /// Creates a new type bridge with heap as the default memory region.
    pub fn new() -> Self {
        TypeBridge {
            struct_cache: HashMap::new(),
            default_region: MemoryRegion::Heap,
        }
    }

    /// Set the default memory region for pointer types
    pub fn set_default_region(&mut self, region: MemoryRegion) {
        self.default_region = region;
    }

    // =========================================================================
    // SOURCE → IR CONVERSION
    // =========================================================================

    /// Convert a source-level Type to an IR-level RegType
    ///
    /// This is the main entry point for bridging the type systems.
    /// It translates high-level type information into low-level memory layout.
    pub fn source_to_ir(&self, source_ty: &Type, ctx: &TypeContext) -> RegType {
        match source_ty {
            // === Primitive Integers ===
            Type::U8 => RegType::Value {
                size: 1,
                signed: false,
            },
            Type::U16 => RegType::Value {
                size: 2,
                signed: false,
            },
            Type::U32 => RegType::Value {
                size: 4,
                signed: false,
            },
            Type::U64 => RegType::Value {
                size: 8,
                signed: false,
            },
            Type::I8 => RegType::Value {
                size: 1,
                signed: true,
            },
            Type::I16 => RegType::Value {
                size: 2,
                signed: true,
            },
            Type::I32 => RegType::Value {
                size: 4,
                signed: true,
            },
            Type::I64 => RegType::Value {
                size: 8,
                signed: true,
            },

            // Floats are also 4/8 byte values
            Type::F32 => RegType::Value {
                size: 4,
                signed: true,
            },
            Type::F64 => RegType::Value {
                size: 8,
                signed: true,
            },

            // Boolean is 1 byte
            Type::Bool => RegType::Bool,

            // Unit is nothing (0 bytes, but we use 8 for uniformity)
            Type::Unit => RegType::Value {
                size: 8,
                signed: false,
            },

            // === Pointer Types ===
            Type::Ptr(inner) => {
                let pointee_size = self.type_size(inner, ctx);
                RegType::Pointer(PointerType {
                    region: self.default_region,
                    bounds: pointee_size.map(|s| (0, s)),
                    struct_type: self.extract_struct_name(inner),
                    offset: 0,
                    alignment: Alignment::from_size(pointee_size.unwrap_or(8)),
                    writable: true,
                })
            }

            Type::Ref(inner) => {
                let pointee_size = self.type_size(inner, ctx);
                RegType::Pointer(PointerType {
                    region: self.default_region,
                    bounds: pointee_size.map(|s| (0, s)),
                    struct_type: self.extract_struct_name(inner),
                    offset: 0,
                    alignment: Alignment::from_size(pointee_size.unwrap_or(8)),
                    writable: false, // Immutable reference
                })
            }

            Type::RefMut(inner) => {
                let pointee_size = self.type_size(inner, ctx);
                RegType::Pointer(PointerType {
                    region: self.default_region,
                    bounds: pointee_size.map(|s| (0, s)),
                    struct_type: self.extract_struct_name(inner),
                    offset: 0,
                    alignment: Alignment::from_size(pointee_size.unwrap_or(8)),
                    writable: true,
                })
            }

            // === Struct Types ===
            Type::Struct(name) => {
                // Structs themselves are values (their size is the struct size)
                if let Some(struct_def) = ctx.lookup_struct(name) {
                    RegType::Value {
                        size: struct_def.total_size as i64,
                        signed: false,
                    }
                } else {
                    RegType::Unknown
                }
            }

            // === Pubkey ===
            Type::Pubkey => {
                // Pubkey is a 32-byte value
                RegType::Value {
                    size: 32,
                    signed: false,
                }
            }

            // === String ===
            Type::String => {
                // String is a pointer to heap-allocated data
                RegType::Pointer(PointerType {
                    region: MemoryRegion::Heap,
                    bounds: None, // Dynamic length
                    struct_type: Some("String".to_string()),
                    offset: 0,
                    alignment: Alignment::Byte1,
                    writable: true,
                })
            }

            // === Array Types ===
            Type::Array { element, size } => {
                // Arrays are values of (element_size * count) bytes
                let elem_size = self.type_size(element, ctx).unwrap_or(8);
                RegType::Value {
                    size: elem_size * (*size as i64),
                    signed: false,
                }
            }

            // === Tuple Types ===
            Type::Tuple(types) => {
                // Tuple size is sum of element sizes
                let total_size: i64 = types
                    .iter()
                    .map(|t| self.type_size(t, ctx).unwrap_or(8))
                    .sum();
                RegType::Value {
                    size: total_size,
                    signed: false,
                }
            }

            // === Function Types ===
            Type::Fn { .. } => {
                // Function pointers are 8-byte values
                RegType::Value {
                    size: 8,
                    signed: false,
                }
            }

            // === Special Types ===
            Type::Any => RegType::Unknown,
            Type::Never => RegType::Unknown,
            Type::Var(_) => RegType::Unknown,
            Type::Unknown => RegType::Unknown,

            // === Refinement Types ===
            // For IR purposes, refinement types are treated as their base type
            // The predicate information is used for verification, not code generation
            Type::Refined(refined) => self.source_to_ir(&refined.base, ctx),
        }
    }

    /// Convert source type to IR type with specific memory region
    ///
    /// Use this when you know the pointer will point to a specific region
    /// (e.g., account data vs heap).
    pub fn source_to_ir_with_region(
        &self,
        source_ty: &Type,
        ctx: &TypeContext,
        region: MemoryRegion,
    ) -> RegType {
        match source_ty {
            Type::Ptr(inner) | Type::Ref(inner) | Type::RefMut(inner) => {
                let pointee_size = self.type_size(inner, ctx);
                let writable = !matches!(source_ty, Type::Ref(_));

                RegType::Pointer(PointerType {
                    region,
                    bounds: pointee_size.map(|s| (0, s)),
                    struct_type: self.extract_struct_name(inner),
                    offset: 0,
                    alignment: Alignment::from_size(pointee_size.unwrap_or(8)),
                    writable,
                })
            }
            _ => self.source_to_ir(source_ty, ctx),
        }
    }

    /// Convert source type to pointer to that type in account data
    ///
    /// This is the most common conversion for struct operations.
    pub fn source_to_account_ptr(
        &self,
        source_ty: &Type,
        ctx: &TypeContext,
        account_idx: u8,
        data_len: Option<i64>,
    ) -> RegType {
        let type_size = self.type_size(source_ty, ctx);
        let struct_name = self
            .extract_struct_name(source_ty)
            .or_else(|| match source_ty {
                Type::Struct(name) => Some(name.clone()),
                _ => None,
            });

        RegType::Pointer(PointerType {
            region: MemoryRegion::AccountData(account_idx),
            bounds: data_len.map(|len| (0, len)).or(type_size.map(|s| (0, s))),
            struct_type: struct_name,
            offset: 0,
            alignment: Alignment::Byte1, // Account data may not be aligned
            writable: true,              // Will be validated by is_writable check
        })
    }

    // =========================================================================
    // IR → SOURCE CONVERSION (for error messages)
    // =========================================================================

    /// Convert an IR-level RegType back to a source-level Type
    ///
    /// Used primarily for generating user-friendly error messages.
    pub fn ir_to_source(&self, ir_ty: &RegType) -> Type {
        match ir_ty {
            RegType::Value { size, signed } => {
                match (size, signed) {
                    (1, false) => Type::U8,
                    (1, true) => Type::I8,
                    (2, false) => Type::U16,
                    (2, true) => Type::I16,
                    (4, false) => Type::U32,
                    (4, true) => Type::I32,
                    (8, false) => Type::U64,
                    (8, true) => Type::I64,
                    (32, false) => Type::Pubkey, // Likely a pubkey
                    _ => Type::Any,              // Unknown size
                }
            }

            RegType::Pointer(ptr) => {
                let inner = if let Some(struct_name) = &ptr.struct_type {
                    Type::Struct(struct_name.clone())
                } else {
                    Type::U8 // Default to byte pointer
                };

                if ptr.writable {
                    Type::Ptr(Box::new(inner))
                } else {
                    Type::Ref(Box::new(inner))
                }
            }

            RegType::Bool => Type::Bool,
            RegType::Unknown => Type::Any,
        }
    }

    // =========================================================================
    // STRUCT CONVERSION
    // =========================================================================

    /// Convert a source-level TypedStructDef to IR-level StructDef
    pub fn source_struct_to_ir(&self, source: &TypedStructDef) -> StructDef {
        use crate::compiler::ir::StructField;

        let fields = source
            .fields
            .iter()
            .map(|f| StructField {
                name: f.name.clone(),
                field_type: self.source_field_type_to_ir(&f.field_type),
                offset: f.offset as i64,
                element_size: None,
                array_count: None,
            })
            .collect();

        StructDef {
            name: source.name.clone(),
            fields,
            total_size: source.total_size as i64,
        }
    }

    /// Convert a source Type to IR FieldType
    fn source_field_type_to_ir(&self, source_ty: &Type) -> FieldType {
        match source_ty {
            Type::U8 => FieldType::Primitive(PrimitiveType::U8),
            Type::U16 => FieldType::Primitive(PrimitiveType::U16),
            Type::U32 => FieldType::Primitive(PrimitiveType::U32),
            Type::U64 => FieldType::Primitive(PrimitiveType::U64),
            Type::I8 => FieldType::Primitive(PrimitiveType::I8),
            Type::I16 => FieldType::Primitive(PrimitiveType::I16),
            Type::I32 => FieldType::Primitive(PrimitiveType::I32),
            Type::I64 => FieldType::Primitive(PrimitiveType::I64),
            Type::Pubkey => FieldType::Pubkey,
            Type::Array { element, size } => {
                if let Some(prim) = self.source_to_primitive(element) {
                    FieldType::Array {
                        element_type: prim,
                        count: *size,
                    }
                } else {
                    // Fallback to u8 array
                    FieldType::Array {
                        element_type: PrimitiveType::U8,
                        count: *size,
                    }
                }
            }
            Type::Struct(name) => FieldType::Struct(name.clone()),
            _ => FieldType::Primitive(PrimitiveType::U64), // Default
        }
    }

    /// Convert source Type to IR PrimitiveType (if applicable)
    fn source_to_primitive(&self, source_ty: &Type) -> Option<PrimitiveType> {
        match source_ty {
            Type::U8 => Some(PrimitiveType::U8),
            Type::U16 => Some(PrimitiveType::U16),
            Type::U32 => Some(PrimitiveType::U32),
            Type::U64 => Some(PrimitiveType::U64),
            Type::I8 => Some(PrimitiveType::I8),
            Type::I16 => Some(PrimitiveType::I16),
            Type::I32 => Some(PrimitiveType::I32),
            Type::I64 => Some(PrimitiveType::I64),
            _ => None,
        }
    }

    // =========================================================================
    // UTILITY FUNCTIONS
    // =========================================================================

    /// Get the size of a source Type in bytes
    pub fn type_size(&self, ty: &Type, ctx: &TypeContext) -> Option<i64> {
        match ty {
            Type::U8 | Type::I8 | Type::Bool => Some(1),
            Type::U16 | Type::I16 => Some(2),
            Type::U32 | Type::I32 | Type::F32 => Some(4),
            Type::U64 | Type::I64 | Type::F64 => Some(8),
            Type::Ptr(_) | Type::Ref(_) | Type::RefMut(_) => Some(8),
            Type::Pubkey => Some(32),
            Type::Struct(name) => ctx.lookup_struct(name).map(|s| s.total_size as i64),
            Type::Array { element, size } => {
                self.type_size(element, ctx).map(|s| s * (*size as i64))
            }
            Type::Tuple(types) => {
                let sizes: Option<Vec<i64>> =
                    types.iter().map(|t| self.type_size(t, ctx)).collect();
                sizes.map(|s| s.iter().sum())
            }
            _ => None,
        }
    }

    /// Extract struct name from a type (for pointer provenance)
    fn extract_struct_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Struct(name) => Some(name.clone()),
            Type::Ptr(inner) | Type::Ref(inner) | Type::RefMut(inner) => {
                self.extract_struct_name(inner)
            }
            _ => None,
        }
    }

    /// Check if a source Type is compatible with an IR RegType
    pub fn types_compatible(&self, source: &Type, ir: &RegType, ctx: &TypeContext) -> bool {
        let converted = self.source_to_ir(source, ctx);

        match (&converted, ir) {
            // Values must match in size
            (RegType::Value { size: s1, .. }, RegType::Value { size: s2, .. }) => s1 == s2,

            // Pointers must match in region and struct type
            (RegType::Pointer(p1), RegType::Pointer(p2)) => {
                p1.region == p2.region && p1.struct_type == p2.struct_type
            }

            // Bool matches bool
            (RegType::Bool, RegType::Bool) => true,

            // Unknown matches anything (gradual typing)
            (RegType::Unknown, _) | (_, RegType::Unknown) => true,

            _ => false,
        }
    }

    /// Import struct definitions from TypeContext into cache
    pub fn import_structs(&mut self, ctx: &TypeContext) {
        // For now, we can't directly iterate TypeContext's structs
        // This would be called with specific struct names
    }

    /// Add a struct definition to the cache
    pub fn add_struct(&mut self, source: &TypedStructDef) {
        let ir_struct = self.source_struct_to_ir(source);
        self.struct_cache.insert(source.name.clone(), ir_struct);
    }

    /// Get cached IR struct definition
    pub fn get_struct(&self, name: &str) -> Option<&StructDef> {
        self.struct_cache.get(name)
    }
}

impl Default for TypeBridge {
    fn default() -> Self {
        Self::new()
    }
}

// =========================================================================
// INTEGRATION HELPERS
// =========================================================================

/// Extension trait for TypeEnv to integrate with source types
pub trait TypeEnvSourceExt {
    /// Set register type from a source Type
    fn set_type_from_source(
        &mut self,
        reg: crate::compiler::ir::IrReg,
        source_ty: &Type,
        ctx: &TypeContext,
        bridge: &TypeBridge,
    );

    /// Validate that a register matches an expected source type
    fn validate_source_type(
        &self,
        reg: crate::compiler::ir::IrReg,
        expected: &Type,
        ctx: &TypeContext,
        bridge: &TypeBridge,
    ) -> bool;
}

impl TypeEnvSourceExt for crate::compiler::ir::TypeEnv {
    fn set_type_from_source(
        &mut self,
        reg: crate::compiler::ir::IrReg,
        source_ty: &Type,
        ctx: &TypeContext,
        bridge: &TypeBridge,
    ) {
        let ir_ty = bridge.source_to_ir(source_ty, ctx);
        self.set_type(reg, ir_ty);
    }

    fn validate_source_type(
        &self,
        reg: crate::compiler::ir::IrReg,
        expected: &Type,
        ctx: &TypeContext,
        bridge: &TypeBridge,
    ) -> bool {
        if let Some(ir_ty) = self.get_type(reg) {
            bridge.types_compatible(expected, ir_ty, ctx)
        } else {
            true // Unknown type is compatible with anything (gradual)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitive_conversion() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        assert!(matches!(
            bridge.source_to_ir(&Type::U64, &ctx),
            RegType::Value {
                size: 8,
                signed: false
            }
        ));

        assert!(matches!(
            bridge.source_to_ir(&Type::I32, &ctx),
            RegType::Value {
                size: 4,
                signed: true
            }
        ));

        assert!(matches!(
            bridge.source_to_ir(&Type::Bool, &ctx),
            RegType::Bool
        ));
    }

    #[test]
    fn test_pointer_conversion() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        let ptr_ty = Type::Ptr(Box::new(Type::U64));
        let ir_ty = bridge.source_to_ir(&ptr_ty, &ctx);

        if let RegType::Pointer(ptr) = ir_ty {
            assert!(ptr.writable);
            assert_eq!(ptr.bounds, Some((0, 8))); // u64 is 8 bytes
        } else {
            panic!("Expected pointer type");
        }
    }

    #[test]
    fn test_immutable_ref() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        let ref_ty = Type::Ref(Box::new(Type::U8));
        let ir_ty = bridge.source_to_ir(&ref_ty, &ctx);

        if let RegType::Pointer(ptr) = ir_ty {
            assert!(!ptr.writable); // Immutable reference
        } else {
            panic!("Expected pointer type");
        }
    }

    #[test]
    fn test_array_size() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        let arr_ty = Type::Array {
            element: Box::new(Type::U8),
            size: 32,
        };
        let ir_ty = bridge.source_to_ir(&arr_ty, &ctx);

        if let RegType::Value { size, .. } = ir_ty {
            assert_eq!(size, 32); // 32 * 1 byte
        } else {
            panic!("Expected value type");
        }
    }

    #[test]
    fn test_account_ptr_conversion() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        let struct_ty = Type::Struct("MyStruct".to_string());
        let ir_ty = bridge.source_to_account_ptr(&struct_ty, &ctx, 0, Some(100));

        if let RegType::Pointer(ptr) = ir_ty {
            assert!(matches!(ptr.region, MemoryRegion::AccountData(0)));
            assert_eq!(ptr.bounds, Some((0, 100)));
            assert_eq!(ptr.struct_type, Some("MyStruct".to_string()));
        } else {
            panic!("Expected pointer type");
        }
    }

    #[test]
    fn test_ir_to_source_roundtrip() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        // Primitive roundtrip
        let source = Type::U64;
        let ir = bridge.source_to_ir(&source, &ctx);
        let back = bridge.ir_to_source(&ir);
        assert_eq!(back, Type::U64);

        // Bool roundtrip
        let source = Type::Bool;
        let ir = bridge.source_to_ir(&source, &ctx);
        let back = bridge.ir_to_source(&ir);
        assert_eq!(back, Type::Bool);
    }

    #[test]
    fn test_type_compatibility() {
        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        // Same types are compatible
        let ir = RegType::Value {
            size: 8,
            signed: false,
        };
        assert!(bridge.types_compatible(&Type::U64, &ir, &ctx));

        // Different sizes are not compatible
        let ir = RegType::Value {
            size: 4,
            signed: false,
        };
        assert!(!bridge.types_compatible(&Type::U64, &ir, &ctx));

        // Unknown is always compatible (gradual typing)
        let ir = RegType::Unknown;
        assert!(bridge.types_compatible(&Type::U64, &ir, &ctx));
    }

    #[test]
    fn test_refinement_type_conversion() {
        use super::super::RefinementType;

        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        // Create a refinement type: {x : u64 | x < 10}
        let refined = RefinementType::bounded_above(Type::U64, 10);
        let refined_type = Type::Refined(Box::new(refined));

        // Should convert to same IR type as the base type
        let ir_ty = bridge.source_to_ir(&refined_type, &ctx);

        assert!(matches!(
            ir_ty,
            RegType::Value {
                size: 8,
                signed: false
            }
        ));

        // Create a refinement type on i32: {x : i32 | x >= 0}
        let refined_i32 = RefinementType::non_negative(Type::I32);
        let refined_type_i32 = Type::Refined(Box::new(refined_i32));

        let ir_ty_i32 = bridge.source_to_ir(&refined_type_i32, &ctx);
        assert!(matches!(
            ir_ty_i32,
            RegType::Value {
                size: 4,
                signed: true
            }
        ));
    }

    #[test]
    fn test_refinement_type_compatibility() {
        use super::super::RefinementType;

        let bridge = TypeBridge::new();
        let ctx = TypeContext::new();

        // Refinement type should be compatible with its base type's IR form
        let refined = RefinementType::range(Type::U64, 0, 100);
        let refined_type = Type::Refined(Box::new(refined));

        // Should match u64's IR type
        let ir = RegType::Value {
            size: 8,
            signed: false,
        };
        assert!(bridge.types_compatible(&refined_type, &ir, &ctx));

        // Should not match i32's IR type (wrong size)
        let ir_wrong = RegType::Value {
            size: 4,
            signed: true,
        };
        assert!(!bridge.types_compatible(&refined_type, &ir_wrong, &ctx));
    }
}
