//! Formal Memory Model for OVSM Compiler
//!
//! This module introduces pointer provenance tracking and type-safe memory operations
//! to eliminate the class of bugs that occur from:
//! - Incorrect load/store sizes
//! - Misaligned memory access
//! - Out-of-bounds account/field access
//! - Type confusion between pointers and values
//!
//! # Design Principles
//!
//! 1. **Typed Registers**: Every register carries type information
//! 2. **Pointer Provenance**: Track what memory region a pointer came from
//! 3. **Bounds Tracking**: Know the valid range for each pointer
//! 4. **Alignment Enforcement**: Validate alignment before load/store
//! 5. **Compile-Time Validation**: Catch errors during IR generation, not at runtime

use std::collections::HashMap;

/// Memory regions in a Solana sBPF program
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MemoryRegion {
    /// Input buffer containing serialized accounts (R1 at entry)
    InputBuffer,
    /// Specific account within input buffer
    Account(u8),
    /// Account's data section (after header)
    AccountData(u8),
    /// Heap memory at 0x300000000+
    Heap,
    /// Account offset table (0x300000000, 8 bytes per account)
    AccountOffsetTable,
    /// CPI data region (0x300000100+)
    CpiRegion,
    /// Event buffer region (0x300001000+)
    EventRegion,
    /// Instruction data (from input buffer)
    InstructionData,
    /// Program ID (from input buffer)
    ProgramId,
    /// Stack frame (local variables)
    Stack,
    /// Unknown/untracked region
    Unknown,
}

/// Alignment requirements for memory access
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Alignment {
    Byte1 = 1,
    Byte2 = 2,
    Byte4 = 4,
    Byte8 = 8,
}

impl Alignment {
    pub fn from_size(size: i64) -> Self {
        match size {
            1 => Alignment::Byte1,
            2 => Alignment::Byte2,
            4 => Alignment::Byte4,
            _ => Alignment::Byte8,
        }
    }

    pub fn value(&self) -> i64 {
        *self as i64
    }
}

/// Type information for a register value
#[derive(Debug, Clone, PartialEq)]
pub enum RegType {
    /// Raw integer value (not a pointer)
    Value {
        /// Size in bytes (1, 2, 4, or 8)
        size: i64,
        /// Whether signed
        signed: bool,
    },

    /// Pointer to memory
    Pointer(PointerType),

    /// Boolean (0 or 1)
    Bool,

    /// Unknown type (from external sources)
    Unknown,
}

impl RegType {
    pub fn u64() -> Self {
        RegType::Value {
            size: 8,
            signed: false,
        }
    }

    pub fn i64() -> Self {
        RegType::Value {
            size: 8,
            signed: true,
        }
    }

    pub fn u8() -> Self {
        RegType::Value {
            size: 1,
            signed: false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self, RegType::Pointer(_))
    }

    pub fn is_value(&self) -> bool {
        matches!(self, RegType::Value { .. })
    }
}

/// Detailed pointer type information
#[derive(Debug, Clone, PartialEq)]
pub struct PointerType {
    /// Which memory region this pointer came from
    pub region: MemoryRegion,

    /// Known bounds of the pointed-to memory (start offset, length)
    /// None means unknown bounds
    pub bounds: Option<(i64, i64)>,

    /// The struct type this points to, if known
    pub struct_type: Option<String>,

    /// Current offset from the base of the region
    pub offset: i64,

    /// Required alignment for dereferencing
    pub alignment: Alignment,

    /// Whether the pointed-to memory is writable
    pub writable: bool,
}

impl PointerType {
    /// Create a pointer to account data
    pub fn account_data(
        account_idx: u8,
        struct_name: Option<String>,
        data_len: Option<i64>,
    ) -> Self {
        PointerType {
            region: MemoryRegion::AccountData(account_idx),
            bounds: data_len.map(|len| (0, len)),
            struct_type: struct_name,
            offset: 0,
            alignment: Alignment::Byte1, // Account data may not be aligned
            writable: true,              // Will be validated separately
        }
    }

    /// Create a pointer to an account field (is_signer, is_writable, etc.)
    pub fn account_field(account_idx: u8, field_offset: i64, field_size: i64) -> Self {
        PointerType {
            region: MemoryRegion::Account(account_idx),
            bounds: Some((field_offset, field_size)),
            struct_type: None,
            offset: field_offset,
            alignment: Alignment::from_size(field_size),
            writable: false, // Account metadata is read-only
        }
    }

    /// Create a pointer to heap memory
    pub fn heap(base_offset: i64, size: Option<i64>) -> Self {
        PointerType {
            region: MemoryRegion::Heap,
            bounds: size.map(|s| (base_offset, s)),
            struct_type: None,
            offset: base_offset,
            alignment: Alignment::Byte8,
            writable: true,
        }
    }

    /// Create a pointer to instruction data
    pub fn instruction_data(len: Option<i64>) -> Self {
        PointerType {
            region: MemoryRegion::InstructionData,
            bounds: len.map(|l| (0, l)),
            struct_type: None,
            offset: 0,
            alignment: Alignment::Byte1,
            writable: false,
        }
    }

    /// Create a pointer to a struct in account data
    pub fn struct_ptr(
        account_idx: u8,
        struct_name: String,
        struct_size: i64,
        data_len: Option<i64>,
    ) -> Self {
        PointerType {
            region: MemoryRegion::AccountData(account_idx),
            bounds: data_len.map(|len| (0, len)).or(Some((0, struct_size))),
            struct_type: Some(struct_name),
            offset: 0,
            alignment: Alignment::Byte1, // Account data may not be aligned
            writable: true,
        }
    }

    /// Create a pointer to a specific field within a struct
    pub fn struct_field_ptr(
        base: &PointerType,
        field_name: String,
        field_offset: i64,
        field_size: i64,
    ) -> Self {
        PointerType {
            region: base.region,
            bounds: Some((field_offset, field_size)),
            struct_type: Some(field_name),
            offset: base.offset + field_offset,
            alignment: Alignment::from_size(field_size),
            writable: base.writable,
        }
    }

    /// Offset this pointer by a constant
    pub fn offset_by(&self, delta: i64) -> Self {
        let mut new = self.clone();
        new.offset += delta;
        new
    }

    /// Offset into a struct field (with type info preserved)
    pub fn field_access(&self, field_offset: i64, field_size: i64, field_name: String) -> Self {
        PointerType {
            region: self.region,
            bounds: Some((self.offset + field_offset, field_size)),
            struct_type: Some(field_name),
            offset: self.offset + field_offset,
            alignment: Alignment::from_size(field_size),
            writable: self.writable,
        }
    }

    /// Check if an access at current offset with given size is in bounds
    pub fn check_bounds(&self, access_size: i64) -> Result<(), MemoryError> {
        if let Some((start, len)) = self.bounds {
            let access_end = self.offset + access_size;
            if self.offset < start || access_end > start + len {
                return Err(MemoryError::OutOfBounds {
                    region: self.region,
                    offset: self.offset,
                    size: access_size,
                    bounds: (start, len),
                });
            }
        }
        Ok(())
    }

    /// Check if alignment is satisfied for an access of given size
    pub fn check_alignment(&self, access_size: i64) -> Result<(), MemoryError> {
        let required = Alignment::from_size(access_size);
        let actual_offset = self.offset;
        if actual_offset % required.value() != 0 {
            return Err(MemoryError::MisalignedAccess {
                region: self.region,
                offset: actual_offset,
                required: required.value(),
                actual: actual_offset % required.value(),
            });
        }
        Ok(())
    }

    /// Check if this pointer allows writing
    pub fn check_writable(&self) -> Result<(), MemoryError> {
        if !self.writable {
            return Err(MemoryError::ReadOnlyWrite {
                region: self.region,
            });
        }
        Ok(())
    }
}

/// Memory access errors caught at compile time
#[derive(Debug, Clone)]
pub enum MemoryError {
    /// Access would be out of bounds
    OutOfBounds {
        region: MemoryRegion,
        offset: i64,
        size: i64,
        bounds: (i64, i64),
    },

    /// Access is misaligned
    MisalignedAccess {
        region: MemoryRegion,
        offset: i64,
        required: i64,
        actual: i64,
    },

    /// Attempting to write to read-only memory
    ReadOnlyWrite { region: MemoryRegion },

    /// Type mismatch: expected pointer but got value (or vice versa)
    TypeMismatch { expected: String, got: String },

    /// Invalid account index
    InvalidAccountIndex { index: u8, max_accounts: u8 },

    /// Field not found in struct
    FieldNotFound {
        struct_name: String,
        field_name: String,
    },

    /// Struct not defined
    StructNotDefined { name: String },

    /// Pointer arithmetic on incompatible pointers
    IncompatiblePointers {
        op: String,
        lhs: MemoryRegion,
        rhs: MemoryRegion,
    },
}

impl std::fmt::Display for MemoryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryError::OutOfBounds {
                region,
                offset,
                size,
                bounds,
            } => {
                write!(
                    f,
                    "Out of bounds access in {:?}: offset {} + size {} exceeds bounds [{}, {})",
                    region,
                    offset,
                    size,
                    bounds.0,
                    bounds.0 + bounds.1
                )
            }
            MemoryError::MisalignedAccess {
                region,
                offset,
                required,
                actual,
            } => {
                write!(f, "Misaligned access in {:?}: offset {} requires {}-byte alignment but has remainder {}",
                       region, offset, required, actual)
            }
            MemoryError::ReadOnlyWrite { region } => {
                write!(f, "Cannot write to read-only region {:?}", region)
            }
            MemoryError::TypeMismatch { expected, got } => {
                write!(f, "Type mismatch: expected {}, got {}", expected, got)
            }
            MemoryError::InvalidAccountIndex {
                index,
                max_accounts,
            } => {
                write!(
                    f,
                    "Invalid account index {}: only {} accounts available",
                    index, max_accounts
                )
            }
            MemoryError::FieldNotFound {
                struct_name,
                field_name,
            } => {
                write!(
                    f,
                    "Field '{}' not found in struct '{}'",
                    field_name, struct_name
                )
            }
            MemoryError::StructNotDefined { name } => {
                write!(f, "Struct '{}' is not defined", name)
            }
            MemoryError::IncompatiblePointers { op, lhs, rhs } => {
                write!(
                    f,
                    "Cannot {} pointers from different regions: {:?} and {:?}",
                    op, lhs, rhs
                )
            }
        }
    }
}

impl std::error::Error for MemoryError {}

/// Typed register that carries type information alongside the register ID
#[derive(Debug, Clone)]
pub struct TypedReg {
    /// The underlying register ID
    pub reg: super::instruction::IrReg,
    /// Type information
    pub ty: RegType,
}

impl TypedReg {
    pub fn new(reg: super::instruction::IrReg, ty: RegType) -> Self {
        TypedReg { reg, ty }
    }

    pub fn value(reg: super::instruction::IrReg, size: i64, signed: bool) -> Self {
        TypedReg {
            reg,
            ty: RegType::Value { size, signed },
        }
    }

    pub fn pointer(reg: super::instruction::IrReg, ptr_type: PointerType) -> Self {
        TypedReg {
            reg,
            ty: RegType::Pointer(ptr_type),
        }
    }

    pub fn bool(reg: super::instruction::IrReg) -> Self {
        TypedReg {
            reg,
            ty: RegType::Bool,
        }
    }

    pub fn unknown(reg: super::instruction::IrReg) -> Self {
        TypedReg {
            reg,
            ty: RegType::Unknown,
        }
    }
}

/// Register type environment tracking types during code generation
pub struct TypeEnv {
    /// Map from register ID to type information
    reg_types: HashMap<u32, RegType>,

    /// Known number of accounts (for bounds checking)
    num_accounts: Option<u8>,

    /// Account writability flags (for write checking)
    account_writable: HashMap<u8, bool>,

    /// Struct definitions for field access validation
    struct_defs: HashMap<String, super::types::StructDef>,

    /// Accumulated errors during type checking
    errors: Vec<MemoryError>,

    /// Whether to enforce strict checking (false = warnings only)
    strict: bool,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            reg_types: HashMap::new(),
            num_accounts: None,
            account_writable: HashMap::new(),
            struct_defs: HashMap::new(),
            errors: Vec::new(),
            strict: true,
        }
    }

    /// Set the known number of accounts
    pub fn set_num_accounts(&mut self, n: u8) {
        self.num_accounts = Some(n);
    }

    /// Set account writability
    pub fn set_account_writable(&mut self, idx: u8, writable: bool) {
        self.account_writable.insert(idx, writable);
    }

    /// Add struct definitions
    pub fn add_struct_defs(&mut self, defs: HashMap<String, super::types::StructDef>) {
        self.struct_defs.extend(defs);
    }

    /// Record the type of a register
    pub fn set_type(&mut self, reg: super::instruction::IrReg, ty: RegType) {
        self.reg_types.insert(reg.0, ty);
    }

    /// Get the type of a register
    pub fn get_type(&self, reg: super::instruction::IrReg) -> Option<&RegType> {
        self.reg_types.get(&reg.0)
    }

    /// Validate and record a typed register
    pub fn record(&mut self, typed: &TypedReg) {
        self.reg_types.insert(typed.reg.0, typed.ty.clone());
    }

    /// Validate that a register holds a pointer
    pub fn expect_pointer(
        &self,
        reg: super::instruction::IrReg,
    ) -> Result<&PointerType, MemoryError> {
        match self.get_type(reg) {
            Some(RegType::Pointer(p)) => Ok(p),
            Some(other) => Err(MemoryError::TypeMismatch {
                expected: "pointer".to_string(),
                got: format!("{:?}", other),
            }),
            None => Err(MemoryError::TypeMismatch {
                expected: "pointer".to_string(),
                got: "unknown".to_string(),
            }),
        }
    }

    /// Validate that a register holds a value
    pub fn expect_value(&self, reg: super::instruction::IrReg) -> Result<(i64, bool), MemoryError> {
        match self.get_type(reg) {
            Some(RegType::Value { size, signed }) => Ok((*size, *signed)),
            Some(other) => Err(MemoryError::TypeMismatch {
                expected: "value".to_string(),
                got: format!("{:?}", other),
            }),
            None => Err(MemoryError::TypeMismatch {
                expected: "value".to_string(),
                got: "unknown".to_string(),
            }),
        }
    }

    /// Validate a memory load operation
    pub fn validate_load(
        &mut self,
        base_reg: super::instruction::IrReg,
        offset: i64,
        load_size: i64,
    ) -> Result<(), MemoryError> {
        if let Some(RegType::Pointer(ptr)) = self.get_type(base_reg).cloned() {
            let access_ptr = ptr.offset_by(offset);
            access_ptr.check_bounds(load_size)?;
            access_ptr.check_alignment(load_size)?;
        }
        // If type unknown, we can't validate - allow but log warning
        Ok(())
    }

    /// Validate a memory store operation
    pub fn validate_store(
        &mut self,
        base_reg: super::instruction::IrReg,
        offset: i64,
        store_size: i64,
    ) -> Result<(), MemoryError> {
        if let Some(RegType::Pointer(ptr)) = self.get_type(base_reg).cloned() {
            let access_ptr = ptr.offset_by(offset);
            access_ptr.check_bounds(store_size)?;
            access_ptr.check_alignment(store_size)?;
            access_ptr.check_writable()?;
        }
        Ok(())
    }

    /// Validate account index access
    pub fn validate_account_index(&self, idx: u8) -> Result<(), MemoryError> {
        if let Some(max) = self.num_accounts {
            if idx >= max {
                return Err(MemoryError::InvalidAccountIndex {
                    index: idx,
                    max_accounts: max,
                });
            }
        }
        Ok(())
    }

    /// Validate struct field access
    /// Returns the field size if valid, records error otherwise
    pub fn validate_struct_field(
        &mut self,
        struct_name: &str,
        field_name: &str,
        base_reg: super::instruction::IrReg,
    ) -> Option<(i64, i64)> {
        // Check if struct is defined
        let struct_def = match self.struct_defs.get(struct_name) {
            Some(def) => def.clone(),
            None => {
                self.record_error(MemoryError::StructNotDefined {
                    name: struct_name.to_string(),
                });
                return None;
            }
        };

        // Check if field exists
        let field = match struct_def.fields.iter().find(|f| f.name == field_name) {
            Some(f) => f.clone(),
            None => {
                self.record_error(MemoryError::FieldNotFound {
                    struct_name: struct_name.to_string(),
                    field_name: field_name.to_string(),
                });
                return None;
            }
        };

        // If base_reg has type info, validate bounds
        if let Some(RegType::Pointer(ptr)) = self.get_type(base_reg).cloned() {
            // Check struct size fits in bounds
            if let Some((start, len)) = ptr.bounds {
                let field_end = field.offset + self.field_size(&field.field_type);
                if field_end > len {
                    self.record_error(MemoryError::OutOfBounds {
                        region: ptr.region,
                        offset: field.offset,
                        size: self.field_size(&field.field_type),
                        bounds: (start, len),
                    });
                }
            }
        }

        Some((field.offset, self.field_size(&field.field_type)))
    }

    /// Get size of a field type
    fn field_size(&self, field_type: &super::types::FieldType) -> i64 {
        use super::types::{FieldType, PrimitiveType};
        match field_type {
            FieldType::Primitive(p) => match p {
                PrimitiveType::U8 | PrimitiveType::I8 => 1,
                PrimitiveType::U16 | PrimitiveType::I16 => 2,
                PrimitiveType::U32 | PrimitiveType::I32 => 4,
                PrimitiveType::U64 | PrimitiveType::I64 => 8,
            },
            FieldType::Pubkey => 32,
            FieldType::Array {
                element_type,
                count,
            } => {
                let elem_size = match element_type {
                    PrimitiveType::U8 | PrimitiveType::I8 => 1,
                    PrimitiveType::U16 | PrimitiveType::I16 => 2,
                    PrimitiveType::U32 | PrimitiveType::I32 => 4,
                    PrimitiveType::U64 | PrimitiveType::I64 => 8,
                };
                elem_size * (*count as i64)
            }
            FieldType::Struct(name) => self
                .struct_defs
                .get(name)
                .map(|s| s.total_size)
                .unwrap_or(0),
        }
    }

    /// Register a struct-typed pointer
    pub fn register_struct_ptr(
        &mut self,
        reg: super::instruction::IrReg,
        struct_name: &str,
        account_idx: Option<u8>,
    ) {
        if let Some(struct_def) = self.struct_defs.get(struct_name) {
            let ptr_type = match account_idx {
                Some(idx) => PointerType::struct_ptr(
                    idx,
                    struct_name.to_string(),
                    struct_def.total_size,
                    None,
                ),
                None => PointerType {
                    region: MemoryRegion::Unknown,
                    bounds: Some((0, struct_def.total_size)),
                    struct_type: Some(struct_name.to_string()),
                    offset: 0,
                    alignment: Alignment::Byte1,
                    writable: true,
                },
            };
            self.set_type(reg, RegType::Pointer(ptr_type));
        }
    }

    /// Record an error (or warning if not strict)
    pub fn record_error(&mut self, err: MemoryError) {
        self.errors.push(err);
    }

    /// Get all accumulated errors
    pub fn errors(&self) -> &[MemoryError] {
        &self.errors
    }

    /// Check if any errors occurred
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty() && self.strict
    }
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

/// Solana account field offsets (compile-time constants)
pub mod account_layout {
    /// Dup info flag (0xFF if new account, else index of duplicate)
    pub const DUP_INFO: i64 = 0;
    /// Is this account a signer?
    pub const IS_SIGNER: i64 = 1;
    /// Is this account writable?
    pub const IS_WRITABLE: i64 = 2;
    /// Is this account executable?
    pub const EXECUTABLE: i64 = 3;
    /// Padding bytes
    pub const PADDING: i64 = 4;
    /// Account public key (32 bytes)
    pub const PUBKEY: i64 = 8;
    pub const PUBKEY_LEN: i64 = 32;
    /// Account owner (32 bytes)
    pub const OWNER: i64 = 40;
    pub const OWNER_LEN: i64 = 32;
    /// Account lamports (8 bytes)
    pub const LAMPORTS: i64 = 72;
    /// Data length (8 bytes)
    pub const DATA_LEN: i64 = 80;
    /// Data start
    pub const DATA: i64 = 88;
    /// Realloc padding
    pub const REALLOC_PADDING: i64 = 10240;
    /// Rent epoch size
    pub const RENT_EPOCH_SIZE: i64 = 8;

    /// Total header size (before data)
    pub const HEADER_SIZE: i64 = 88;
}

/// Heap region layout (compile-time constants)
pub mod heap_layout {
    /// Base heap address in Solana sBPF
    pub const HEAP_BASE: i64 = 0x300000000;
    /// Account offset table (8 bytes per account, max 64 accounts = 512 bytes)
    pub const ACCOUNT_TABLE_OFFSET: i64 = 0;
    pub const ACCOUNT_TABLE_SIZE: i64 = 512; // 64 accounts * 8 bytes
    /// CPI data region
    pub const CPI_OFFSET: i64 = 0x100; // 256 bytes after base
    pub const CPI_SIZE: i64 = 0xF00; // ~4KB for CPI
    /// Event buffer region
    pub const EVENT_OFFSET: i64 = 0x1000; // 4KB after base
    pub const EVENT_SIZE: i64 = 0x1000; // 4KB for events
    /// Scratch space
    pub const SCRATCH_OFFSET: i64 = 0x2000;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pointer_bounds_check() {
        let ptr = PointerType::account_data(0, None, Some(100));
        assert!(ptr.check_bounds(50).is_ok());
        assert!(ptr.check_bounds(100).is_ok());
        assert!(ptr.check_bounds(101).is_err());

        let offset_ptr = ptr.offset_by(50);
        assert!(offset_ptr.check_bounds(50).is_ok());
        assert!(offset_ptr.check_bounds(51).is_err());
    }

    #[test]
    fn test_alignment_check() {
        let ptr = PointerType::heap(0, Some(1000));
        assert!(ptr.check_alignment(1).is_ok()); // 1-byte always aligned
        assert!(ptr.check_alignment(8).is_ok()); // 0 % 8 == 0

        let offset_ptr = ptr.offset_by(1);
        assert!(offset_ptr.check_alignment(1).is_ok()); // 1-byte always ok
        assert!(offset_ptr.check_alignment(2).is_err()); // 1 % 2 != 0
        assert!(offset_ptr.check_alignment(4).is_err()); // 1 % 4 != 0
    }

    #[test]
    fn test_writable_check() {
        let data_ptr = PointerType::account_data(0, None, None);
        assert!(data_ptr.check_writable().is_ok());

        let field_ptr = PointerType::account_field(0, account_layout::IS_SIGNER, 1);
        assert!(field_ptr.check_writable().is_err());
    }

    #[test]
    fn test_type_env() {
        use super::super::instruction::IrReg;

        let mut env = TypeEnv::new();
        env.set_num_accounts(3);

        // Valid account index
        assert!(env.validate_account_index(0).is_ok());
        assert!(env.validate_account_index(2).is_ok());

        // Invalid account index
        assert!(env.validate_account_index(3).is_err());
        assert!(env.validate_account_index(255).is_err());
    }
}
