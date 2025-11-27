# OVSM sBPF Compiler Built-in Functions

Reference guide for built-in functions available when compiling OVSM to Solana sBPF bytecode.

**Version:** 1.0.7
**Last Updated:** 2025-11-27
**Tested On:** Solana Devnet

---

## Table of Contents

1. [Overview](#overview)
2. [Account Access Functions](#account-access-functions)
3. [Memory Operations](#memory-operations)
4. [Instruction Data](#instruction-data)
5. [Cross-Program Invocation (CPI)](#cross-program-invocation-cpi)
6. [Logging Syscalls](#logging-syscalls)
7. [Control Flow](#control-flow)
8. [Arithmetic Operations](#arithmetic-operations)
9. [Comparison Operations](#comparison-operations)
10. [Built-in Variables](#built-in-variables)
11. [Complete Examples](#complete-examples)
12. [Solana Account Memory Layout](#solana-account-memory-layout)

---

## Overview

The OVSM sBPF compiler transforms OVSM LISP code into Solana BPF bytecode that runs on-chain. Unlike the interpreter (which runs locally), compiled programs:

- Execute within Solana's BPF virtual machine
- Have access to account data passed by the runtime
- Use syscalls for logging, cryptography, and CPI
- Must stay within compute unit budgets

### Compilation Command

```bash
# Compile OVSM to sBPF
osvm ovsm compile program.ovsm -o program.so

# Deploy to Solana
solana program deploy program.so --keypair wallet.json --url devnet

# Invoke the program
# (Use a client or test script)
```

---

## Account Access Functions

These functions read data from accounts passed to your program.

### `num-accounts`

**Signature:** `(num-accounts)`
**Description:** Get the number of accounts passed to the program
**Returns:** u64 - Number of accounts
**Tested:** ✅ Verified on devnet

```lisp
;; Check if we have at least 2 accounts
(if (>= (num-accounts) 2)
    (sol_log_ "Got enough accounts")
    (sol_log_ "Need more accounts"))
```

---

### `account-lamports`

**Signature:** `(account-lamports idx)`
**Description:** Get the lamport balance of account at index
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** u64 - Lamports (1 SOL = 1,000,000,000 lamports)
**Tested:** ✅ Verified on devnet

```lisp
;; Log the balance of the first account
(sol_log_ "Account 0 lamports:")
(sol_log_64_ (account-lamports 0))
```

**Example Output:**
```
Program log: Account 0 lamports:
Program log: 0x6e97a9a0, ...  ; ~1.85 SOL
```

---

### `account-data-len`

**Signature:** `(account-data-len idx)`
**Description:** Get the length of account data in bytes
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** u64 - Data length in bytes
**Tested:** ✅ Verified on devnet

```lisp
;; Check if account has data
(if (> (account-data-len 0) 0)
    (sol_log_ "Account has data")
    (sol_log_ "Account is empty"))
```

---

### `account-data-ptr`

**Signature:** `(account-data-ptr idx)`
**Description:** Get pointer to the start of account data
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** Pointer to account data
**Note:** Use with `mem-load` to read data

```lisp
;; Get pointer to account data and read first 8 bytes
(define data-ptr (account-data-ptr 0))
(define first-u64 (mem-load data-ptr 0))
(sol_log_64_ first-u64)
```

---

### `account-pubkey`

**Signature:** `(account-pubkey idx)`
**Description:** Get pointer to the 32-byte public key of account
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** Pointer to 32-byte pubkey
**Note:** Use with `sol_log_pubkey` to display
**Tested:** ✅ Verified on devnet

```lisp
;; Log the pubkey of account 0
(define pk-ptr (account-pubkey 0))
(sol_log_ "Account pubkey:")
(sol_log_pubkey pk-ptr)
```

**Example Output:**
```
Program log: Account pubkey:
Program log: HzqH2YWBcYfxecnxWwszpwypKBBYdjgxq1ANB7DSkKV
```

---

### `account-owner`

**Signature:** `(account-owner idx)`
**Description:** Get pointer to the 32-byte owner pubkey of account
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** Pointer to 32-byte owner pubkey
**Tested:** ✅ Verified on devnet

```lisp
;; Log the owner of account 0
(define owner-ptr (account-owner 0))
(sol_log_ "Account owner:")
(sol_log_pubkey owner-ptr)
```

**Example Output:**
```
Program log: Account owner:
Program log: 11111111111111111111111111111111  ; System Program
```

---

### `account-is-signer`

**Signature:** `(account-is-signer idx)`
**Description:** Check if account signed the transaction
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** 1 if signer, 0 if not
**Tested:** ✅ Verified on devnet

```lisp
;; Verify account 0 is a signer
(if (account-is-signer 0)
    (sol_log_ "Account is signer")
    (do
      (sol_log_ "ERROR: Account must sign")
      1))  ; Return error
```

---

### `account-is-writable`

**Signature:** `(account-is-writable idx)`
**Description:** Check if account is writable
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** 1 if writable, 0 if not
**Tested:** ✅ Verified on devnet

```lisp
;; Verify account 0 is writable before modifying
(if (account-is-writable 0)
    (sol_log_ "Account is writable")
    (do
      (sol_log_ "ERROR: Account must be writable")
      1))  ; Return error
```

---

## Memory Operations

Low-level memory access for reading/writing account data.

### `mem-load`

**Signature:** `(mem-load ptr offset)`
**Description:** Load 8 bytes (u64) from memory
**Parameters:**
- `ptr` - Base pointer
- `offset` - Byte offset from pointer (constant)
**Returns:** u64 value at ptr+offset
**Tested:** ✅ Verified on devnet

```lisp
;; Read first 8 bytes of account data
(define data-ptr (account-data-ptr 0))
(define value (mem-load data-ptr 0))
(sol_log_64_ value)

;; Read bytes 8-15
(define value2 (mem-load data-ptr 8))
```

---

### `mem-load1`

**Signature:** `(mem-load1 ptr offset)`
**Description:** Load 1 byte from memory (zero-extended to u64)
**Parameters:**
- `ptr` - Base pointer
- `offset` - Byte offset from pointer (constant)
**Returns:** u8 value at ptr+offset (as u64)

```lisp
;; Read discriminator byte (common in Anchor programs)
(define data-ptr (account-data-ptr 0))
(define discriminator (mem-load1 data-ptr 0))
(if (= discriminator 1)
    (sol_log_ "Account type: Initialized")
    (sol_log_ "Account type: Unknown"))
```

---

### `mem-store`

**Signature:** `(mem-store base offset value)`
**Description:** Store 8 bytes (u64) to memory
**Parameters:**
- `base` - Base pointer
- `offset` - Byte offset from pointer (constant)
- `value` - u64 value to store
**Returns:** None
**Note:** Account must be writable!

```lisp
;; Write value to account data
(define data-ptr (account-data-ptr 0))
(mem-store data-ptr 0 42)  ; Store 42 at offset 0
```

---

## Instruction Data

Functions for accessing instruction data passed to your program.

### `instruction-data-len`

**Signature:** `(instruction-data-len)`
**Description:** Get the length of instruction data in bytes
**Returns:** u64 - Number of bytes of instruction data
**Tested:** ✅ Verified on devnet

```lisp
;; Check if we have enough instruction data
(if (>= (instruction-data-len) 8)
    (sol_log_ "Got enough data")
    (sol_log_ "Need at least 8 bytes"))
```

---

### `instruction-data-ptr`

**Signature:** `(instruction-data-ptr)`
**Description:** Get pointer to instruction data buffer
**Returns:** u64 - Pointer to instruction data
**Tested:** ✅ Verified on devnet

```lisp
;; Read first 8 bytes as u64 from instruction data
(define amount (mem-load (instruction-data-ptr) 0))
(sol_log_ "Amount from instruction:")
(sol_log_64_ amount)
```

---

## Cross-Program Invocation (CPI)

Functions for calling other Solana programs from your OVSM program.

> **Note:** CPI currently has a known issue with heap address loading. The functions compile and deploy correctly, but may fail at runtime due to register spilling of large constant addresses. This is being actively fixed.

### `system-transfer`

**Signature:** `(system-transfer src_idx dest_idx amount)`
**Description:** Transfer SOL from one account to another via System Program CPI
**Parameters:**
- `src_idx` - Account index of source (must be signer)
- `dest_idx` - Account index of destination
- `amount` - Lamports to transfer
**Returns:** u64 - 0 on success, error code on failure
**Status:** ⚠️ Compiles but runtime issue pending fix

```lisp
;; Transfer 0.001 SOL from account 0 to account 1
(define result (system-transfer 0 1 1000000))
(if (= result 0)
    (sol_log_ "Transfer successful!")
    (sol_log_ "Transfer failed"))
```

---

### `invoke`

**Signature:** `(invoke instruction-ptr account-infos-ptr num-accounts)`
**Description:** Low-level CPI for calling any program with custom instruction
**Parameters:**
- `instruction-ptr` - Pointer to SolInstruction struct (40 bytes)
- `account-infos-ptr` - Pointer to account infos array
- `num-accounts` - Number of accounts
**Returns:** u64 - 0 on success, error code on failure
**Status:** ⚠️ Advanced use - requires manual struct building

```lisp
;; For advanced users who build their own instruction structures
(define result (invoke instr-ptr accts-ptr 2))
```

---

### `invoke-signed`

**Signature:** `(invoke-signed instr-ptr acct-infos-ptr num-accts signers-seeds-ptr num-signers)`
**Description:** PDA-signed CPI for program-derived address signing
**Parameters:**
- `instr-ptr` - Pointer to SolInstruction struct
- `acct-infos-ptr` - Pointer to account infos
- `num-accts` - Number of accounts
- `signers-seeds-ptr` - Pointer to signer seeds array
- `num-signers` - Number of PDA signers
**Returns:** u64 - 0 on success, error code on failure
**Status:** ⚠️ Advanced use - requires PDA seed setup

---

### CPI Data Structures Reference

When building custom CPI instructions, use these memory layouts:

**SolInstruction (40 bytes):**
```
+0:  u64 program_id_ptr     ; Pointer to 32-byte program ID
+8:  u64 accounts_ptr       ; Pointer to SolAccountMeta array
+16: u64 account_len        ; Number of accounts
+24: u64 data_ptr           ; Pointer to instruction data
+32: u64 data_len           ; Length of instruction data
```

**SolAccountMeta (16 bytes):**
```
+0:  u64 pubkey_ptr         ; Pointer to 32-byte pubkey
+8:  u8  is_writable        ; 1 if writable, 0 otherwise
+9:  u8  is_signer          ; 1 if signer, 0 otherwise
+10: padding (6 bytes)
```

**System Program Transfer Instruction Data (12 bytes):**
```
+0:  u32 instruction_index  ; 2 for Transfer
+4:  u64 lamports           ; Amount to transfer
```

---

## Logging Syscalls

Functions for logging to Solana program logs.

### `sol_log_`

**Signature:** `(sol_log_ message)`
**Description:** Log a string message
**Parameters:**
- `message` - String literal
**Returns:** Syscall result (usually 0)
**Tested:** ✅ Verified on devnet

```lisp
(sol_log_ "Hello from OVSM!")
(sol_log_ "=== Program Start ===")
```

**Example Output:**
```
Program log: Hello from OVSM!
Program log: === Program Start ===
```

---

### `sol_log_64_`

**Signature:** `(sol_log_64_ val1 [val2 val3 val4 val5])`
**Description:** Log up to 5 numeric values
**Parameters:**
- `val1` through `val5` - u64 values (1-5 args)
**Returns:** Syscall result
**Tested:** ✅ Verified on devnet

```lisp
;; Log single value
(sol_log_64_ (account-lamports 0))

;; Log multiple values
(sol_log_64_ 1 2 3 4 5)
```

**Example Output:**
```
Program log: 0x6e97a9a0, 0x15, 0x1000003d8, 0x1e, 0x30
```

---

### `sol_log_pubkey`

**Signature:** `(sol_log_pubkey ptr)`
**Description:** Log a 32-byte public key in base58 format
**Parameters:**
- `ptr` - Pointer to 32-byte pubkey
**Returns:** Syscall result
**Tested:** ✅ Verified on devnet

```lisp
;; Log account pubkey
(sol_log_pubkey (account-pubkey 0))

;; Log account owner
(sol_log_pubkey (account-owner 0))
```

**Example Output:**
```
Program log: HzqH2YWBcYfxecnxWwszpwypKBBYdjgxq1ANB7DSkKV
```

---

## Control Flow

### `do`

**Signature:** `(do expr1 expr2 ... exprN)`
**Description:** Execute expressions sequentially, return last value
**Returns:** Value of last expression

```lisp
(do
  (sol_log_ "Step 1")
  (sol_log_ "Step 2")
  0)  ; Return success
```

---

### `if`

**Signature:** `(if condition then-expr else-expr)`
**Description:** Conditional execution
**Returns:** Value of branch taken

```lisp
(if (>= (num-accounts) 1)
    (sol_log_ "Got accounts")
    (sol_log_ "No accounts"))
```

---

### `while`

**Signature:** `(while condition body...)`
**Description:** Loop while condition is true
**Returns:** null

```lisp
(define i 0)
(while (< i 5)
  (sol_log_64_ i)
  (set! i (+ i 1)))
```

---

### `define`

**Signature:** `(define name value)`
**Description:** Define a variable
**Returns:** The value

```lisp
(define balance (account-lamports 0))
(define pk-ptr (account-pubkey 0))
```

---

### `set!`

**Signature:** `(set! name value)`
**Description:** Mutate an existing variable
**Returns:** The new value

```lisp
(define counter 0)
(set! counter (+ counter 1))
```

---

## Arithmetic Operations

### `+` (Addition)

**Signature:** `(+ a b)`
**Description:** Add two numbers
**Returns:** Sum

```lisp
(+ 10 20)  ; => 30
```

---

### `-` (Subtraction)

**Signature:** `(- a b)`
**Description:** Subtract b from a
**Returns:** Difference

```lisp
(- 100 25)  ; => 75
```

---

### `*` (Multiplication)

**Signature:** `(* a b)`
**Description:** Multiply two numbers
**Returns:** Product

```lisp
(* 6 7)  ; => 42
```

---

### `/` (Division)

**Signature:** `(/ a b)`
**Description:** Integer division
**Returns:** Quotient

```lisp
(/ 100 3)  ; => 33
```

---

### `%` (Modulo)

**Signature:** `(% a b)`
**Description:** Remainder of division
**Returns:** Remainder

```lisp
(% 17 5)  ; => 2
```

---

## Comparison Operations

### `=` (Equal)

**Signature:** `(= a b)`
**Returns:** 1 if equal, 0 if not

---

### `!=` (Not Equal)

**Signature:** `(!= a b)`
**Returns:** 1 if not equal, 0 if equal

---

### `<` (Less Than)

**Signature:** `(< a b)`
**Returns:** 1 if a < b, 0 otherwise

---

### `<=` (Less Than or Equal)

**Signature:** `(<= a b)`
**Returns:** 1 if a <= b, 0 otherwise

---

### `>` (Greater Than)

**Signature:** `(> a b)`
**Returns:** 1 if a > b, 0 otherwise

---

### `>=` (Greater Than or Equal)

**Signature:** `(>= a b)`
**Returns:** 1 if a >= b, 0 otherwise

---

## Built-in Variables

These variables are automatically available in every OVSM program.

### `accounts`

**Type:** Pointer
**Description:** Base pointer to serialized accounts data
**Populated:** At program entry from R1

```lisp
;; accounts is implicitly used by account-* functions
;; You rarely need to access it directly
```

---

### `instruction-data`

**Type:** Pointer
**Description:** Pointer to instruction data
**Populated:** At program entry from R2

```lisp
;; Check if instruction data exists
(if instruction-data
    (sol_log_ "Got instruction data")
    (sol_log_ "No instruction data"))
```

---

## Complete Examples

### Example 1: Basic Account Inspector

```lisp
;; Inspect account passed to the program
(do
  (sol_log_ "=== Account Inspector ===")

  ;; Log number of accounts
  (sol_log_ "Number of accounts:")
  (sol_log_64_ (num-accounts))

  ;; Log account 0 details
  (sol_log_ "Account 0 pubkey:")
  (sol_log_pubkey (account-pubkey 0))

  (sol_log_ "Account 0 owner:")
  (sol_log_pubkey (account-owner 0))

  (sol_log_ "Account 0 lamports:")
  (sol_log_64_ (account-lamports 0))

  (sol_log_ "Account 0 is-signer:")
  (sol_log_64_ (account-is-signer 0))

  (sol_log_ "Account 0 is-writable:")
  (sol_log_64_ (account-is-writable 0))

  (sol_log_ "=== Inspection Complete ===")
  0)  ; Return success
```

---

### Example 2: Signer Verification

```lisp
;; Verify the first account signed the transaction
(do
  (sol_log_ "=== Signer Verification ===")

  ;; Check we have at least 1 account
  (if (< (num-accounts) 1)
      (do
        (sol_log_ "ERROR: Need at least 1 account")
        1)  ; Return error
      (do
        ;; Check if account 0 is signer
        (if (account-is-signer 0)
            (do
              (sol_log_ "Verification passed!")
              (sol_log_ "Signer pubkey:")
              (sol_log_pubkey (account-pubkey 0))
              0)  ; Success
            (do
              (sol_log_ "ERROR: Account 0 must sign")
              1)))))  ; Error
```

---

### Example 3: Reading Account Data

```lisp
;; Read and log data from an account
(do
  (sol_log_ "=== Reading Account Data ===")

  (define data-len (account-data-len 0))
  (sol_log_ "Data length:")
  (sol_log_64_ data-len)

  (if (> data-len 0)
      (do
        (define data-ptr (account-data-ptr 0))

        ;; Read first 8 bytes as u64
        (sol_log_ "First 8 bytes:")
        (sol_log_64_ (mem-load data-ptr 0))

        ;; If data is long enough, read more
        (if (>= data-len 16)
            (do
              (sol_log_ "Bytes 8-15:")
              (sol_log_64_ (mem-load data-ptr 8)))
            null))
      (sol_log_ "Account has no data"))

  0)
```

---

## Solana Account Memory Layout

Understanding how Solana serializes accounts is crucial for the compiler.

### Input Buffer Layout

When your program is invoked, accounts are serialized at the input pointer (R1):

```
Offset  Size  Field
------  ----  -----
0       8     num_accounts (u64)

For each account:
+0      1     dup_info (u8, 0xFF if not duplicate)
+1      1     is_signer (u8, 0 or 1)
+2      1     is_writable (u8, 0 or 1)
+3      1     executable (u8, 0 or 1)
+4      4     padding (4 bytes)
+8      32    pubkey (32 bytes)
+40     32    owner (32 bytes)
+72     8     lamports (u64)
+80     8     data_len (u64)
+88     N     data (variable, padded to 8-byte boundary)
+88+N   10240 MAX_PERMITTED_DATA_INCREASE padding
+...    8     rent_epoch (u64)
```

### Key Offsets (from account start)

| Field | Offset | Size |
|-------|--------|------|
| dup_info | 0 | 1 |
| is_signer | 1 | 1 |
| is_writable | 2 | 1 |
| executable | 3 | 1 |
| padding | 4-7 | 4 |
| pubkey | 8 | 32 |
| owner | 40 | 32 |
| lamports | 72 | 8 |
| data_len | 80 | 8 |
| data | 88 | variable |

---

## Compute Units

Every operation costs compute units. Default budget: 200,000 CU.

| Operation | Approximate CU |
|-----------|----------------|
| `sol_log_` | ~100 |
| `sol_log_64_` | ~100 |
| `sol_log_pubkey` | ~100 |
| Arithmetic | ~1 |
| Memory load | ~1 |
| Memory store | ~1 |
| Comparison | ~1 |

### Estimated CU Calculator

The compiler reports estimated CU usage:
```
✅ Compiled successfully!
   ...
   Estimated CU: 1103
```

---

## Error Handling

### Return Values

- Return `0` for success
- Return non-zero (typically `1`) for errors

```lisp
(if (some-error-condition)
    (do
      (sol_log_ "ERROR: Something went wrong")
      1)  ; Return error
    (do
      (sol_log_ "Success!")
      0))  ; Return success
```

### Common Errors

| Error | Cause | Fix |
|-------|-------|-----|
| AccessViolation | Reading invalid memory | Check account index bounds |
| ComputationalBudgetExceeded | Too many operations | Optimize or request more CU |
| InvalidAccountData | Wrong account layout | Verify account structure |

---

## New in v1.0.5 (Session 2)

### `set-lamports`

**Signature:** `(set-lamports idx value)`
**Description:** Set the lamport balance of account at index (for SOL transfers)
**Parameters:**
- `idx` - Account index (0-based)
- `value` - New lamport value (u64)
**Returns:** None
**Note:** Account must be writable and owned by your program or System Program

```lisp
;; Transfer SOL: reduce account 0, increase account 1
(define current-0 (account-lamports 0))
(define current-1 (account-lamports 1))
(define transfer-amount 1000000000)  ;; 1 SOL

(set-lamports 0 (- current-0 transfer-amount))
(set-lamports 1 (+ current-1 transfer-amount))
```

---

### `account-executable`

**Signature:** `(account-executable idx)`
**Description:** Check if account is an executable program
**Parameters:**
- `idx` - Account index (0-based)
**Returns:** 1 if executable, 0 if not
**Tested:** ✅ Verified on devnet

```lisp
;; Verify account is a program
(if (account-executable 0)
    (sol_log_ "Account is executable")
    (sol_log_ "Account is not a program"))
```

---

### `instruction-data-len`

**Signature:** `(instruction-data-len)`
**Description:** Get the length of instruction data passed to the program
**Returns:** u64 - Length in bytes
**Tested:** ✅ Verified on devnet
**Limitation:** Assumes all accounts have zero data (common case for wallet accounts)

```lisp
;; Check instruction data length
(define len (instruction-data-len))
(sol_log_ "Instruction data length:")
(sol_log_64_ len)
```

---

### `instruction-data-ptr`

**Signature:** `(instruction-data-ptr)`
**Description:** Get pointer to the instruction data buffer
**Returns:** Pointer to instruction data
**Tested:** ✅ Verified on devnet
**Limitation:** Assumes all accounts have zero data

```lisp
;; Read instruction data
(define ptr (instruction-data-ptr))
(define first-byte (mem-load1 ptr 0))
(sol_log_ "First byte of instruction data:")
(sol_log_64_ first-byte)
```

**Example: Parsing Instruction Data**
```lisp
(do
  (define len (instruction-data-len))

  (if (>= len 8)
      (do
        (define ptr (instruction-data-ptr))
        ;; Read first 8 bytes as u64 argument
        (define arg1 (mem-load ptr 0))
        (sol_log_ "Argument 1:")
        (sol_log_64_ arg1))
      (sol_log_ "Not enough instruction data"))
  0)
```

---

## Future Features (Planned)

- [ ] Cross-Program Invocation (CPI)
- [ ] Program Derived Addresses (PDAs)
- [ ] Token Program integration
- [ ] Custom error codes

---

## Additional Resources

- **[BUILTIN_FUNCTIONS.md](BUILTIN_FUNCTIONS.md)** - Interpreter built-ins
- **[README.md](README.md)** - OVSM overview
- **[Solana Docs](https://docs.solana.com/developing/on-chain-programs/overview)** - Solana program development

---

**Last Updated:** 2025-11-26
**OVSM Version:** 1.0.6
**Compiler Status:** Production-ready for account reading and instruction data

---

*Made with ❤️ by the OpenSVM team*

*OVSM: Solana programs in LISP* 🚀
