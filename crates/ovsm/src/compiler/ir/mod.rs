//! # Intermediate Representation (IR) for OVSM Compilation
//!
//! This module compiles OVSM LISP to a three-address-code IR, which is then
//! lowered to Solana BPF (sBPF) bytecode by the codegen phase.
//!
//! ## Module Structure
//!
//! ```text
//! ir/
//! ├── mod.rs          # This file - module definition and re-exports
//! ├── types.rs        # PrimitiveType, FieldType, StructField, StructDef
//! ├── instruction.rs  # IrReg, IrInstruction enum (3AC operations)
//! ├── program.rs      # BasicBlock, IrProgram (CFG representation)
//! └── generator.rs    # IrGenerator with all macro implementations (~5700 lines)
//! ```
//!
//! ## Key Types
//!
//! - [`IrReg`] - Virtual register (infinite supply, mapped to physical during codegen)
//! - [`IrInstruction`] - Three-address-code instruction (arithmetic, memory, control flow)
//! - [`IrProgram`] - Complete IR program with instructions, blocks, and string table
//! - [`IrGenerator`] - AST-to-IR transformer with 60+ macro implementations
//!
//! ## Macro Categories (in generator.rs)
//!
//! | Category | Macros |
//! |----------|--------|
//! | Struct | `define-struct`, `struct-get/set/size/ptr/offset/field-size/idl` |
//! | Borsh | `borsh-serialize`, `borsh-deserialize`, `borsh-size` |
//! | Account | `account-data-ptr/len`, `account-lamports/pubkey/owner` |
//! | Assertions | `assert-signer`, `assert-writable`, `assert-owner`, `is-signer/writable` |
//! | Zerocopy | `zerocopy-load`, `zerocopy-store` |
//! | System CPI | `system-transfer`, `system-create-account`, `system-allocate`, `system-assign` |
//! | SPL Token | `spl-token-transfer`, `spl-token-mint-to`, `spl-token-burn`, `spl-close-account` |
//! | Signed CPI | `*-signed` variants for PDA authority |
//! | PDA | `derive-pda`, `create-pda`, `get-ata`, `find-pda` |
//! | PDA Cache | `pda-cache-init`, `pda-cache-store`, `pda-cache-lookup` |
//! | Events | `emit-event`, `emit-log` |
//! | Sysvars | `clock-unix-timestamp`, `clock-epoch`, `rent-minimum-balance` |
//! | Errors | `anchor-error`, `require`, `msg` |
//!
//! ## Memory Model (NEW)
//!
//! The `memory_model` module provides formal pointer provenance tracking and
//! compile-time memory safety validation:
//!
//! - [`TypedReg`] - Register with type information (value vs pointer)
//! - [`PointerType`] - Detailed pointer metadata (region, bounds, alignment)
//! - [`TypeEnv`] - Type environment for tracking register types during codegen
//! - [`MemoryError`] - Compile-time memory access errors
//!
//! This enables catching bugs like misaligned loads, out-of-bounds access,
//! and type confusion at compile time rather than runtime.

mod generator;
mod instruction;
pub mod memory_model;
mod program;
mod types;

// Re-export all public types
pub use generator::IrGenerator;
pub use instruction::{IrInstruction, IrReg};
pub use program::{BasicBlock, IrProgram};
pub use types::{FieldType, PrimitiveType, StructDef, StructField};

// Re-export memory model types
pub use memory_model::{
    account_layout, heap_layout, Alignment, MemoryError, MemoryRegion, PointerType, RegType,
    TypeEnv, TypedReg,
};
