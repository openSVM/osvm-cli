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

mod types;
mod instruction;
mod program;
mod generator;

// Re-export all public types
pub use types::{PrimitiveType, FieldType, StructField, StructDef};
pub use instruction::{IrReg, IrInstruction};
pub use program::{BasicBlock, IrProgram};
pub use generator::IrGenerator;
