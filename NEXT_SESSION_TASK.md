# Next Session Task: Implement Solana Syscall Support

**Date:** November 24, 2025
**Priority:** HIGH
**Status:** Ready to start

## Context

✅ **COMPLETED:** ELF alignment issues are fully resolved. Programs now load successfully with solana-rbpf.

❌ **BLOCKER:** Current error when deploying to devnet:
```
Error: ELF error: ELF error: Unresolved symbol (sol_log_64) at instruction #46 (ELF file offset 0x174)
```

## The Problem

The OVSM compiler generates syscall instructions but doesn't properly implement the Solana syscall mechanism. When we use `sol_log_64` in OVSM code, the compiler needs to:

1. Generate proper SBPF call instructions with `imm=-1` (syscall marker)
2. Create relocations (R_BPF_64_32) for syscall addresses
3. Add syscall symbols to `.dynsym` section
4. Ensure symbols are in `.dynstr` with correct names

## Current State

### What Works ✅
- ELF structure is correct
- Alignment is perfect (8-byte aligned `.rel.dyn`)
- solana-rbpf loads the ELF without errors
- Relocations are generated and linked properly

### What's Missing ❌
- Syscall symbol resolution
- Proper syscall name mapping
- Syscall function imports in dynamic section

## Test Case

**Test Program:** `/tmp/test_with_syscall.ovsm`
```lisp
(do
  (sol_log_64 42 0 0 0 0)
  0)
```

**Generated ELF:** `/tmp/test_aligned_final.so` (1248 bytes)
**Current Error:** "Unresolved symbol (sol_log_64)"

## Investigation Steps

1. **Compare with Reference Program**
   ```bash
   # Working reference program
   readelf -s /tmp/reference_program/target/deploy/reference_program.so | grep sol_

   # Our program
   readelf -s /tmp/test_aligned_final.so | grep sol_
   ```

2. **Check Dynamic Symbol Table**
   ```bash
   readelf --dyn-syms /tmp/test_aligned_final.so
   ```

3. **Examine Relocations**
   ```bash
   readelf -r /tmp/test_aligned_final.so
   ```

## Files to Modify

### Primary File
**`crates/ovsm/src/compiler/elf.rs`**
- Function: `write()` (around line 125)
- Section: Syscall handling (around line 180-230)
- Key area: `.dynsym` and `.dynstr` generation (lines 470-485)

### Current Syscall Implementation

Look for where syscalls are collected and processed:
```rust
// Around line 180-230 in elf.rs
let mut syscalls = Vec::new();
for (offset, inst) in program.iter().enumerate() {
    if inst.is_syscall() {
        syscalls.push(SyscallInfo {
            offset: offset * 8,
            name: inst.syscall_name(),
        });
    }
}
```

### Expected Symbol Format

Solana expects syscall symbols like:
- `sol_log_`
- `sol_log_64_`
- `sol_invoke_signed_rust`
- etc.

Note the **trailing underscore** in symbol names!

## Debugging Commands

```bash
# 1. Build and test
cargo build --release
./target/release/osvm ovsm compile --output /tmp/test_syscall.so /tmp/test_with_syscall.ovsm

# 2. Verify alignment still works
python3 << 'EOF'
import struct
with open("/tmp/test_syscall.so", 'rb') as f:
    data = f.read()
# Check .rel.dyn alignment
e_shoff = struct.unpack('<Q', data[0x28:0x30])[0]
# ... (alignment check code)
EOF

# 3. Test with solana-rbpf
cd /tmp/elf_test
sed -i 's|/tmp/.*\.so|/tmp/test_syscall.so|' src/main.rs
cargo run

# 4. Try deploying
solana program deploy /tmp/test_syscall.so \
  --program-id /tmp/test_deploy_keys/program-id.json \
  --keypair /tmp/test_deploy_keys/fee-payer.json \
  --url devnet
```

## Expected Fix

The fix will likely involve:

1. **Adding proper syscall name mapping:**
   ```rust
   fn get_solana_syscall_name(name: &str) -> String {
       format!("{}_", name)  // Add trailing underscore
   }
   ```

2. **Ensuring symbols are in `.dynsym`:**
   ```rust
   for syscall in &syscalls {
       // Add to dynsym with proper STT_FUNC type
       // Add to dynstr with trailing underscore
   }
   ```

3. **Verify relocation type:**
   ```rust
   // Should be R_BPF_64_32 (type 10) for V1 syscalls
   let r_info = ((sym_idx as u64) << 32) | (R_BPF_64_32 as u64);
   ```

## Success Criteria

✅ `solana program deploy` succeeds without "Unresolved symbol" error
✅ Program deploys to devnet
✅ Program can be invoked (even if it does nothing)
✅ `solana-rbpf` loads and validates the program

## Reference Materials

- **Working ELF:** `/tmp/reference_program/target/deploy/reference_program.so`
- **Solana SBPF Spec:** Check Solana docs for syscall conventions
- **Test Program:** `/tmp/test_with_syscall.ovsm`
- **Alignment Fix Report:** `ALIGNMENT_FIX_SUCCESS.md`
- **Binary Comparison:** `BINARY_COMPARISON_ANALYSIS.md`

## Quick Start for Next Session

```bash
# 1. Check current state
cd /home/larp/larpdevs/osvm-cli
git status

# 2. Look at syscall implementation
code crates/ovsm/src/compiler/elf.rs
# Search for "syscall" and examine the implementation

# 3. Compare with reference
readelf -s /tmp/reference_program/target/deploy/reference_program.so | grep -E "sol_|Symbol table"
readelf -s /tmp/test_aligned_final.so | grep -E "sol_|Symbol table"

# 4. Start implementing the fix
```

## Notes

- **DO NOT touch alignment code** - it's working perfectly
- **Focus only on syscall symbol resolution**
- The ELF structure is correct; we just need proper syscall imports
- Test frequently with both `solana-rbpf` and `solana program deploy`

## Timeline Estimate

**Expected Time:** 2-4 hours
- 30 min: Investigation and comparison with reference
- 1-2 hours: Implementation
- 30 min: Testing and verification
- 30 min: Documentation

---

**Status:** Ready to start
**Blocking:** None (all dependencies resolved)
**Risk:** Low (clear path forward)
