# Chapter 4: Compiling OVSM to sBPF

## 4.1 Introduction: From LISP to Blockchain Bytecode

This chapter documents the complete pipeline for compiling OVSM LISP source code into sBPF (Solana Berkeley Packet Filter) bytecode executable on the Solana blockchain. While previous chapters focused on OVSM as an interpreted language for algorithmic trading, this chapter demonstrates how OVSM strategies can be compiled to trustless, on-chain programs that execute without intermediaries.

**Why Compile to sBPF?**

The motivation for sBPF compilation extends beyond mere technical curiosity:

1. **Trustless Execution**: On-chain programs execute deterministically without relying on off-chain infrastructure
2. **Composability**: sBPF programs can interact with DeFi protocols, oracles, and other on-chain components
3. **Verifiability**: Anyone can audit the deployed bytecode and verify it matches the source OVSM
4. **MEV Resistance**: On-chain execution eliminates front-running vectors present in off-chain order submission
5. **24/7 Operation**: No server maintenance, no downtime, no custody risk

**Chapter Organization:**

- **Section 4.2**: sBPF Architecture and Virtual Machine Model
- **Section 4.3**: OVSM-to-sBPF Compilation Pipeline
- **Section 4.4**: Intermediate Representation (IR) Design
- **Section 4.5**: Code Generation and Optimization
- **Section 4.6**: Memory Model Mapping and Data Layout
- **Section 4.7**: Syscall Integration and Cross-Program Invocation (CPI)
- **Section 4.8**: Compute Unit Budgeting and Optimization
- **Section 4.9**: Deployment, Testing, and Verification
- **Section 4.10**: Complete Worked Example: Pairs Trading On-Chain

This chapter assumes familiarity with OVSM language features (Chapter 3) and basic blockchain concepts. Readers implementing compilers should consult the formal semantics in Chapter 3; practitioners deploying strategies can focus on Sections 4.9-4.10.

---

## 4.2 sBPF Architecture and Virtual Machine Model

### 4.2.1 The Berkeley Packet Filter Legacy

The Berkeley Packet Filter (BPF) originated in 1992 as a virtual machine for efficient packet filtering in operating system kernels. Its design philosophy—register-based execution, minimal instruction set, verifiable safety—made it ideal for untrusted code execution. Solana adapted BPF for blockchain smart contracts, creating sBPF with blockchain-specific extensions.

**Key Architectural Differences: sBPF vs EVM**

| Feature | sBPF (Solana) | EVM (Ethereum) |
|---------|---------------|----------------|
| **Architecture** | Register-based (11 registers) | Stack-based (256-word stack) |
| **Instruction Set** | RISC-like, ~100 opcodes | CISC-like, ~140 opcodes |
| **Memory Model** | Separate heap/stack, bounds-checked | Single memory space, gas-metered |
| **Compute Limits** | 200K-1.4M compute units | 30M gas per block |
| **Verification** | Static analysis before execution | Runtime checks with revert |
| **Parallelism** | Account-based parallel execution | Sequential block processing |

### 4.2.2 sBPF Register Model

sBPF provides 11 general-purpose registers, each 64 bits wide:

```
r0  - Return value register (function return, syscall results)
r1  - Function argument 1 (or general-purpose)
r2  - Function argument 2 (or general-purpose)
r3  - Function argument 3 (or general-purpose)
r4  - Function argument 4 (or general-purpose)
r5  - Function argument 5 (or general-purpose)
r6-r9 - Callee-saved registers (preserved across function calls)
r10 - Frame pointer (read-only, points to stack frame base)
r11 - Program counter (implicit, not directly accessible)
```

**Register Allocation Strategy:**

Our OVSM compiler uses the following allocation strategy:

- **r0**: Expression evaluation results, return values
- **r1-r5**: Function arguments (up to 5 parameters)
- **r6**: Environment pointer (access to OVSM runtime context)
- **r7**: Heap pointer (current allocation frontier)
- **r8-r9**: Temporary registers for complex expressions
- **r10**: Stack frame base (managed by VM)

### 4.2.3 Memory Layout

sBPF programs operate on four distinct memory regions:

```
┌─────────────────────────────────────────┐
│  Program Code (.text)                   │  ← Read-only instructions
│  Max: 10KB-100KB depending on compute   │
├─────────────────────────────────────────┤
│  Read-Only Data (.rodata)               │  ← String literals, constants
│  Max: 10KB                              │
├─────────────────────────────────────────┤
│  Stack (grows downward)                 │  ← Local variables, call frames
│  Size: 4KB fixed                        │
│  r10 points here                        │
├─────────────────────────────────────────┤
│  Heap (grows upward)                    │  ← Dynamic allocations
│  Size: 32KB default (configurable)     │
│  Allocated via sol_alloc() syscall      │
└─────────────────────────────────────────┘
```

**Critical Constraints:**

1. **Stack Limit**: 4KB hard limit, no dynamic expansion
2. **Heap Fragmentation**: No garbage collection during transaction
3. **Memory Alignment**: All loads/stores must be naturally aligned
4. **Bounds Checking**: VM verifies all memory accesses at load time

### 4.2.4 Instruction Format

sBPF instructions are 64 bits (8 bytes), encoded as:

```
┌────────┬────────┬────────┬────────┬─────────────────────┐
│ opcode │  dst   │  src   │ offset │     immediate       │
│ 8 bits │ 4 bits │ 4 bits │ 16 bits│     32 bits         │
└────────┴────────┴────────┴────────┴─────────────────────┘
```

**Example: Add Two Registers**

```assembly
add64 r1, r2    ; r1 = r1 + r2

Encoding:
opcode = 0x0f (ALU64_ADD_REG)
dst    = 0x1  (r1)
src    = 0x2  (r2)
offset = 0x0
imm    = 0x0

Bytes: 0f 21 00 00 00 00 00 00
```

### 4.2.5 Syscalls and Cross-Program Invocation

sBPF programs interact with the Solana runtime through syscalls:

**Core Syscalls for OVSM:**

```rust
// Memory management
sol_alloc(size: u64) -> *mut u8
sol_free(ptr: *mut u8, size: u64)

// Logging and debugging
sol_log(message: &str)
sol_log_64(v1: u64, v2: u64, v3: u64, v4: u64, v5: u64)

// Cryptography
sol_sha256(data: &[u8], hash_result: &mut [u8; 32])
sol_keccak256(data: &[u8], hash_result: &mut [u8; 32])

// Cross-Program Invocation (CPI)
sol_invoke_signed(
    instruction: &Instruction,
    account_infos: &[AccountInfo],
    signers_seeds: &[&[&[u8]]]
) -> Result<()>

// Clock and timing
sol_get_clock_sysvar(clock: &mut Clock)
sol_get_epoch_schedule_sysvar(epoch_schedule: &mut EpochSchedule)

// Account data access
sol_get_return_data() -> Option<(Pubkey, Vec<u8>)>
sol_set_return_data(data: &[u8])
```

---

## 4.3 OVSM-to-sBPF Compilation Pipeline

### 4.3.1 Pipeline Overview

The OVSM compiler transforms source code through six phases:

```
┌──────────────┐
│  OVSM Source │  (define x (+ 1 2))
│   (.ovsm)    │
└──────┬───────┘
       │
       ▼
┌──────────────┐
│   Scanner    │  Tokenization: (LPAREN, DEFINE, IDENT, ...)
└──────┬───────┘
       │
       ▼
┌──────────────┐
│    Parser    │  AST: DefineNode(Ident("x"), AddNode(...))
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Type Checker │  Infer types, verify correctness
│  (optional)  │  x: i64, type-safe addition
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  IR Generator│  Three-address code:
│              │  t1 = const 1
│              │  t2 = const 2
│              │  t3 = add t1, t2
│              │  x = t3
└──────┬───────┘
       │
       ▼
┌──────────────┐
│  Optimizer   │  Constant folding:
│              │  t3 = const 3
│              │  x = t3
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ Code Generator│ sBPF bytecode:
│              │  mov64 r1, 3
│              │  stxdw [r10-8], r1
└──────┬───────┘
       │
       ▼
┌──────────────┐
│ sBPF Binary  │  .so (ELF shared object)
│   (.so)      │  Ready for deployment
└──────────────┘
```

### 4.3.2 Scanner: OVSM Lexical Analysis

The scanner (Chapter 3.2) tokenizes OVSM source into a stream of tokens. For sBPF compilation, we extend the scanner to track source location metadata for better error messages:

```rust
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub location: SourceLocation,
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}
```

This metadata enables stack traces that map sBPF program counters back to OVSM source lines.

### 4.3.3 Parser: Building the Abstract Syntax Tree

The parser (Chapter 3.3) constructs an AST representation of the OVSM program. For compilation, we use a typed AST variant:

```rust
#[derive(Debug, Clone)]
pub enum Expr {
    // Literals
    IntLiteral(i64, SourceLocation),
    FloatLiteral(f64, SourceLocation),
    StringLiteral(String, SourceLocation),
    BoolLiteral(bool, SourceLocation),

    // Variables and bindings
    Identifier(String, SourceLocation),
    Define(String, Box<Expr>, SourceLocation),
    Set(String, Box<Expr>, SourceLocation),

    // Control flow
    If(Box<Expr>, Box<Expr>, Box<Expr>, SourceLocation),
    While(Box<Expr>, Box<Expr>, SourceLocation),
    For(String, Box<Expr>, Box<Expr>, SourceLocation),

    // Functions
    Lambda(Vec<String>, Box<Expr>, SourceLocation),
    FunctionCall(Box<Expr>, Vec<Expr>, SourceLocation),

    // Operators (desugared to function calls)
    BinaryOp(BinOp, Box<Expr>, Box<Expr>, SourceLocation),
    UnaryOp(UnOp, Box<Expr>, SourceLocation),

    // Sequences
    Do(Vec<Expr>, SourceLocation),
}
```

### 4.3.4 Type Checking (Optional Phase)

sBPF is statically typed at the bytecode level. The type checker infers OVSM types and ensures operations are well-typed:

```rust
pub enum OvsmType {
    I64,           // 64-bit integer
    F64,           // 64-bit float (emulated in sBPF)
    Bool,          // Boolean (represented as i64: 0/1)
    String,        // UTF-8 string (heap-allocated)
    Array(Box<OvsmType>),  // Homogeneous array
    Object(HashMap<String, OvsmType>), // Key-value map
    Function(Vec<OvsmType>, Box<OvsmType>), // Function type
    Any,           // Dynamic type (requires runtime checks)
}
```

**Type Inference Example:**

```lisp
(define x 42)          ; Inferred: x: I64
(define y (+ x 10))    ; Inferred: y: I64 (+ requires I64 operands)
(define z "hello")     ; Inferred: z: String
```

**Type Error Detection:**

```lisp
(+ 10 "hello")  ; Error: Type mismatch
                ; Expected: (I64, I64) -> I64
                ; Found: (I64, String)
```

---

## 4.4 Intermediate Representation (IR) Design

### 4.4.1 Three-Address Code

We use a three-address code (3AC) IR, where each instruction has at most three operands:

```
result = operand1 op operand2
```

**Example Transformation:**

```lisp
;; OVSM source
(define total (+ (* price quantity) tax))

;; Three-address code IR
t1 = mul price, quantity
t2 = add t1, tax
total = t2
```

### 4.4.2 IR Instruction Set

```rust
#[derive(Debug, Clone)]
pub enum IrInstruction {
    // Constants
    ConstI64(IrReg, i64),
    ConstF64(IrReg, f64),
    ConstBool(IrReg, bool),
    ConstString(IrReg, String),

    // Arithmetic
    Add(IrReg, IrReg, IrReg),      // dst = src1 + src2
    Sub(IrReg, IrReg, IrReg),
    Mul(IrReg, IrReg, IrReg),
    Div(IrReg, IrReg, IrReg),
    Mod(IrReg, IrReg, IrReg),

    // Comparison
    Eq(IrReg, IrReg, IrReg),       // dst = (src1 == src2)
    Ne(IrReg, IrReg, IrReg),
    Lt(IrReg, IrReg, IrReg),
    Le(IrReg, IrReg, IrReg),
    Gt(IrReg, IrReg, IrReg),
    Ge(IrReg, IrReg, IrReg),

    // Logical
    And(IrReg, IrReg, IrReg),
    Or(IrReg, IrReg, IrReg),
    Not(IrReg, IrReg),

    // Control flow
    Label(String),
    Jump(String),
    JumpIf(IrReg, String),         // Jump if src != 0
    JumpIfNot(IrReg, String),

    // Function calls
    Call(Option<IrReg>, String, Vec<IrReg>), // dst = func(args...)
    Return(Option<IrReg>),

    // Memory operations
    Load(IrReg, IrReg, i64),       // dst = *(src + offset)
    Store(IrReg, IrReg, i64),      // *(dst + offset) = src
    Alloc(IrReg, IrReg),           // dst = alloc(size)

    // Syscalls
    Syscall(Option<IrReg>, String, Vec<IrReg>),
}
```

### 4.4.3 Control Flow Graph (CFG)

The IR is organized into basic blocks connected by a control flow graph:

```rust
#[derive(Debug)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<IrInstruction>,
    pub successors: Vec<String>,  // Labels of successor blocks
    pub predecessors: Vec<String>, // Labels of predecessor blocks
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub entry: String,
    pub blocks: HashMap<String, BasicBlock>,
}
```

**Example CFG for If Statement:**

```lisp
;; OVSM source
(if (> x 10)
    (log :message "big")
    (log :message "small"))

;; Control Flow Graph
┌─────────────────┐
│  entry_block    │
│  t1 = x > 10    │
│  jumpif t1, L1  │
└────┬───────┬────┘
     │       │
     │       └─────────────┐
     ▼                     ▼
┌─────────────┐      ┌─────────────┐
│     L1      │      │     L2      │
│ log "big"   │      │ log "small" │
└────┬────────┘      └────┬────────┘
     │                    │
     └──────────┬─────────┘
                ▼
          ┌─────────────┐
          │  exit_block │
          └─────────────┘
```

---

## 4.5 Code Generation and Optimization

### 4.5.1 Register Allocation

We use a simple linear-scan register allocator:

```rust
pub struct RegisterAllocator {
    // Physical sBPF registers available for allocation
    available: Vec<SbpfReg>,

    // Mapping from IR virtual registers to sBPF physical registers
    allocation: HashMap<IrReg, SbpfReg>,

    // Spill slots on stack for register pressure
    spill_slots: HashMap<IrReg, i64>,
}

impl RegisterAllocator {
    pub fn new() -> Self {
        Self {
            // r0 reserved for return values
            // r1-r5 reserved for function arguments
            // r6-r9 available for allocation
            available: vec![
                SbpfReg::R6,
                SbpfReg::R7,
                SbpfReg::R8,
                SbpfReg::R9,
            ],
            allocation: HashMap::new(),
            spill_slots: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, ir_reg: IrReg) -> SbpfReg {
        if let Some(&physical) = self.allocation.get(&ir_reg) {
            return physical;
        }

        if let Some(physical) = self.available.pop() {
            self.allocation.insert(ir_reg, physical);
            physical
        } else {
            // Spill to stack
            self.spill(ir_reg)
        }
    }

    fn spill(&mut self, ir_reg: IrReg) -> SbpfReg {
        // Find least-recently-used register and spill it
        // This is a simplified implementation
        let victim = SbpfReg::R6;
        let offset = self.spill_slots.len() as i64 * 8;
        self.spill_slots.insert(ir_reg, offset);
        victim
    }
}
```

### 4.5.2 Instruction Selection

Each IR instruction maps to one or more sBPF instructions:

```rust
fn emit_add(&mut self, dst: IrReg, src1: IrReg, src2: IrReg) {
    let dst_reg = self.allocate(dst);
    let src1_reg = self.allocate(src1);
    let src2_reg = self.allocate(src2);

    // Move src1 to dst if different
    if dst_reg != src1_reg {
        self.emit(SbpfInstr::Mov64Reg(dst_reg, src1_reg));
    }

    // dst = dst + src2
    self.emit(SbpfInstr::Add64Reg(dst_reg, src2_reg));
}
```

**Common Instruction Mappings:**

| IR Instruction | sBPF Instruction(s) |
|----------------|---------------------|
| `Add(r1, r2, r3)` | `mov64 r1, r2; add64 r1, r3` |
| `Sub(r1, r2, r3)` | `mov64 r1, r2; sub64 r1, r3` |
| `Mul(r1, r2, r3)` | `mov64 r1, r2; mul64 r1, r3` |
| `Div(r1, r2, r3)` | `mov64 r1, r2; div64 r1, r3` |
| `ConstI64(r1, 42)` | `mov64 r1, 42` |
| `Load(r1, r2, 8)` | `ldxdw r1, [r2+8]` |
| `Store(r1, r2, 0)` | `stxdw [r1], r2` |
| `Jump(L1)` | `ja L1` |
| `JumpIf(r1, L1)` | `jne r1, 0, L1` |

### 4.5.3 Optimization Passes

The compiler applies several optimization passes to the IR:

**1. Constant Folding**

```lisp
;; Before
(define x (+ 2 3))

;; IR before optimization
t1 = const 2
t2 = const 3
t3 = add t1, t2
x = t3

;; IR after optimization
x = const 5
```

**2. Dead Code Elimination**

```lisp
;; Before
(define x 10)
(define y 20)  ; Never used
(log :value x)

;; IR after DCE
x = const 10
syscall log, x
;; y assignment eliminated
```

**3. Common Subexpression Elimination (CSE)**

```lisp
;; Before
(define a (* x y))
(define b (+ (* x y) 10))

;; IR after CSE
t1 = mul x, y
a = t1
t2 = add t1, 10  ; Reuse t1 instead of recomputing
b = t2
```

**4. Loop Invariant Code Motion (LICM)**

```lisp
;; Before
(while (< i 100)
  (define limit (* 10 5))  ; Invariant
  (if (< i limit)
      (log :value i)
      null)
  (set! i (+ i 1)))

;; After LICM
(define limit (* 10 5))  ; Moved outside loop
(while (< i 100)
  (if (< i limit)
      (log :value i)
      null)
  (set! i (+ i 1)))
```

---

## 4.6 Memory Model Mapping and Data Layout

### 4.6.1 Value Representation

OVSM values are represented in sBPF using tagged unions:

```rust
// 64-bit value representation
// Bits 0-55:  Payload (56 bits)
// Bits 56-63: Type tag (8 bits)

pub const TAG_I64: u64    = 0x00 << 56;
pub const TAG_F64: u64    = 0x01 << 56;
pub const TAG_BOOL: u64   = 0x02 << 56;
pub const TAG_NULL: u64   = 0x03 << 56;
pub const TAG_STRING: u64 = 0x04 << 56;  // Payload is heap pointer
pub const TAG_ARRAY: u64  = 0x05 << 56;  // Payload is heap pointer
pub const TAG_OBJECT: u64 = 0x06 << 56;  // Payload is heap pointer
```

**Example Encoding:**

```
Integer 42:
  Value: 0x00_0000_0000_0000_002A
  Bits:  [00000000][00000000_00000000_00000000_00101010]
          ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          TAG_I64   Payload = 42

Boolean true:
  Value: 0x02_0000_0000_0000_0001
  Bits:  [00000010][00000000_00000000_00000000_00000001]
          ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          TAG_BOOL  Payload = 1

String "hello" (heap pointer 0x1000):
  Value: 0x04_0000_0000_0000_1000
  Bits:  [00000100][00000000_00000000_00000000_00001000]
          ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
          TAG_STRING Payload = heap address
```

### 4.6.2 Heap Data Structures

**String Layout:**

```
┌────────────┬────────────────┬──────────────────┐
│   Length   │   Capacity     │   UTF-8 Bytes    │
│  (8 bytes) │   (8 bytes)    │   (n bytes)      │
└────────────┴────────────────┴──────────────────┘
```

**Array Layout:**

```
┌────────────┬────────────────┬──────────────────┐
│   Length   │   Capacity     │   Elements       │
│  (8 bytes) │   (8 bytes)    │   (n * 8 bytes)  │
└────────────┴────────────────┴──────────────────┘
```

**Object Layout (Hash Map):**

```
┌────────────┬────────────────┬──────────────────┐
│   Size     │   Capacity     │   Buckets        │
│  (8 bytes) │   (8 bytes)    │   (n * Entry)    │
└────────────┴────────────────┴──────────────────┘

Entry:
┌────────────┬────────────────┬──────────────────┐
│   Key Ptr  │   Value        │   Next Ptr       │
│  (8 bytes) │   (8 bytes)    │   (8 bytes)      │
└────────────┴────────────────┴──────────────────┘
```

### 4.6.3 Stack Frame Layout

Each function call creates a stack frame:

```
High addresses
┌─────────────────────────────┐
│  Return address             │  ← Saved by call instruction
├─────────────────────────────┤
│  Saved r6                   │  ← Callee-saved registers
│  Saved r7                   │
│  Saved r8                   │
│  Saved r9                   │
├─────────────────────────────┤
│  Local variable 1           │  ← Function locals
│  Local variable 2           │
│  ...                        │
├─────────────────────────────┤
│  Spill slots                │  ← Register spills
└─────────────────────────────┘  ← r10 (frame pointer)
Low addresses
```

**Frame Access Example:**

```assembly
; Access local variable at offset -16 from frame pointer
ldxdw r1, [r10-16]  ; Load local var into r1

; Store to local variable
stxdw [r10-24], r2  ; Store r2 to local var
```

---

## 4.7 Syscall Integration and Cross-Program Invocation

### 4.7.1 Logging from OVSM

The `log` function in OVSM compiles to `sol_log` syscalls:

```lisp
;; OVSM source
(log :message "Price:" :value price)

;; Generated sBPF
mov64 r1, str_offset  ; Pointer to "Price:"
mov64 r2, str_len
call sol_log_         ; Syscall

mov64 r1, price       ; Value to log
call sol_log_64       ; Syscall
```

### 4.7.2 Cross-Program Invocation (CPI)

OVSM can invoke other Solana programs via CPI:

```lisp
;; OVSM source: Swap tokens on Raydium
(define swap-instruction
  (raydium-swap
    :pool pool-address
    :amount-in 1000000
    :minimum-amount-out 900000))

(sol-invoke swap-instruction accounts signers)
```

**Generated sBPF:**

```assembly
; Build Instruction struct on stack
mov64 r1, program_id
stxdw [r10-8], r1

mov64 r1, accounts_ptr
stxdw [r10-16], r1

mov64 r1, data_ptr
stxdw [r10-24], r1

; Call sol_invoke_signed
mov64 r1, r10
sub64 r1, 24          ; Pointer to Instruction
mov64 r2, account_infos
mov64 r3, signers_seeds
call sol_invoke_signed_
```

### 4.7.3 Oracle Integration

Reading Pyth or Switchboard price oracles:

```lisp
;; OVSM source
(define btc-price (pyth-get-price btc-oracle-account))

;; Generated sBPF
mov64 r1, btc_oracle_account
call get_account_data

; Parse Pyth price struct
ldxdw r1, [r0+PRICE_OFFSET]    ; Load price
ldxdw r2, [r0+EXPO_OFFSET]     ; Load exponent
ldxdw r3, [r0+CONF_OFFSET]     ; Load confidence

; Adjust by exponent: price * 10^expo
call apply_exponent
mov64 btc_price, r0
```

---

## 4.8 Compute Unit Budgeting and Optimization

### 4.8.1 Compute Unit Model

Every sBPF instruction consumes compute units (CUs). Solana transactions have a compute budget:

- **Default**: 200,000 CU per instruction
- **Maximum**: 1,400,000 CU per instruction (with priority fee)
- **Per-account writable lock**: 12,000 CU

**Common Instruction Costs:**

| Instruction | Compute Units |
|-------------|---------------|
| `mov64` | 1 CU |
| `add64`, `sub64` | 1 CU |
| `mul64` | 5 CU |
| `div64` | 20 CU |
| `syscall` | 100-5000 CU (depends on syscall) |
| `sha256` (64 bytes) | 200 CU |
| `sol_invoke` | 1000-5000 CU |

### 4.8.2 Static Analysis for CU Estimation

The compiler estimates compute usage:

```rust
pub fn estimate_compute_units(cfg: &ControlFlowGraph) -> u64 {
    let mut total_cu = 0;

    for block in cfg.blocks.values() {
        let mut block_cu = 0;

        for instr in &block.instructions {
            block_cu += match instr {
                IrInstruction::Add(..) => 1,
                IrInstruction::Mul(..) => 5,
                IrInstruction::Div(..) => 20,
                IrInstruction::Syscall(_, name, _) => {
                    match name.as_str() {
                        "sol_log" => 100,
                        "sol_sha256" => 200,
                        "sol_invoke_signed" => 5000,
                        _ => 1000,
                    }
                }
                _ => 1,
            };
        }

        total_cu += block_cu;
    }

    total_cu
}
```

**Optimization: Compute Budget Request**

If estimate exceeds 200K CU, insert budget request:

```rust
// Request 1.4M compute units
let budget_ix = ComputeBudgetInstruction::set_compute_unit_limit(1_400_000);
transaction.add_instruction(budget_ix);
```

### 4.8.3 Hotspot Analysis

The compiler identifies expensive code paths:

```lisp
;; Expensive: Division in tight loop
(while (< i 1000)
  (define ratio (/ total i))  ; 20 CU per iteration
  (log :value ratio)          ; 100 CU per iteration
  (set! i (+ i 1)))

; Total: 1000 * (20 + 100 + 1) = 121,000 CU
```

**Optimization: Hoist invariant division**

```lisp
;; Optimized: Move division outside loop
(define ratio (/ total 1))
(while (< i 1000)
  (log :value ratio)
  (set! i (+ i 1)))

; Total: 20 + 1000 * (100 + 1) = 101,020 CU (17% reduction)
```

---

## 4.9 Deployment, Testing, and Verification

### 4.9.1 Compilation Workflow

```bash
# Compile OVSM to sBPF
osvm ovsm compile strategy.ovsm \
  --output strategy.so \
  --optimize 2 \
  --target bpf

# Verify output is valid ELF
file strategy.so
# Output: strategy.so: ELF 64-bit LSB shared object, eBPF

# Disassemble for inspection
llvm-objdump -d strategy.so > strategy.asm

# Deploy to Solana devnet
solana program deploy strategy.so \
  --url devnet \
  --keypair deployer.json

# Output: Program Id: 7XZo...ABC
```

### 4.9.2 Unit Testing sBPF Programs

Test individual functions using the sBPF simulator:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use solana_program_test::*;

    #[tokio::test]
    async fn test_calculate_spread() {
        // Setup program test environment
        let program_id = Pubkey::new_unique();
        let mut program_test = ProgramTest::new(
            "ovsm_pairs_trading",
            program_id,
            processor!(process_instruction),
        );

        // Create test accounts
        let price_a = 100_000_000; // $100.00 (8 decimals)
        let price_b = 101_000_000; // $101.00

        program_test.add_account(
            price_a_account,
            Account {
                lamports: 1_000_000,
                data: price_a.to_le_bytes().to_vec(),
                owner: program_id,
                ..Account::default()
            },
        );

        // Start test context
        let (mut banks_client, payer, recent_blockhash) =
            program_test.start().await;

        // Build transaction
        let mut transaction = Transaction::new_with_payer(
            &[calculate_spread_instruction(price_a_account, price_b_account)],
            Some(&payer.pubkey()),
        );
        transaction.sign(&[&payer], recent_blockhash);

        // Execute and verify
        banks_client.process_transaction(transaction).await.unwrap();

        // Check result
        let spread_account = banks_client
            .get_account(spread_result_account)
            .await
            .unwrap()
            .unwrap();

        let spread = i64::from_le_bytes(spread_account.data[0..8].try_into().unwrap());
        assert_eq!(spread, 1_000_000); // $1.00 spread
    }
}
```

### 4.9.3 Integration Testing

Test full strategy execution on localnet:

```bash
# Start local validator
solana-test-validator \
  --reset \
  --bpf-program strategy.so

# Deploy strategy
solana program deploy strategy.so

# Run integration test
cargo test --test integration_pairs_trading
```

**Integration Test Example:**

```rust
#[tokio::test]
async fn test_pairs_trading_workflow() {
    // 1. Deploy strategy program
    let program_id = deploy_program("strategy.so").await;

    // 2. Initialize strategy state
    let strategy_account = initialize_strategy(
        program_id,
        asset_a,
        asset_b,
        cointegration_params,
    ).await;

    // 3. Simulate price movements
    update_oracle_price(asset_a, 100_00); // $100.00
    update_oracle_price(asset_b, 102_00); // $102.00 (spread = 2%)

    // 4. Trigger strategy execution
    let result = execute_strategy(strategy_account).await;

    // 5. Verify trade executed
    assert!(result.trade_executed);
    assert_eq!(result.position, Position::Short(asset_b, 1000));
    assert_eq!(result.position, Position::Long(asset_a, 1000));

    // 6. Simulate mean reversion
    update_oracle_price(asset_b, 101_00); // Spread narrows to 1%

    // 7. Trigger exit
    let exit_result = execute_strategy(strategy_account).await;
    assert!(exit_result.position_closed);

    // 8. Verify profit
    let pnl = calculate_pnl(strategy_account).await;
    assert!(pnl > 0); // Profitable trade
}
```

### 4.9.4 Formal Verification

Use symbolic execution to verify safety properties:

```rust
// Property: Strategy never exceeds max position size
#[verify]
fn test_position_size_bounded() {
    let strategy = kani::any::<Strategy>();
    let market_state = kani::any::<MarketState>();

    let new_position = strategy.calculate_position(&market_state);

    assert!(new_position.size <= MAX_POSITION_SIZE);
}

// Property: Strategy never divides by zero
#[verify]
fn test_no_division_by_zero() {
    let price_a = kani::any::<u64>();
    let price_b = kani::any::<u64>();

    kani::assume(price_a > 0);
    kani::assume(price_b > 0);

    let ratio = calculate_ratio(price_a, price_b);
    // Should never panic
}
```

Run verification:

```bash
cargo kani --function test_position_size_bounded
# Output: VERIFICATION SUCCESSFUL
```

---

## 4.10 Complete Worked Example: Pairs Trading On-Chain

### 4.10.1 Strategy Overview

We'll compile a simplified pairs trading strategy to sBPF that:

1. Monitors price oracles for two cointegrated assets (SOL/USDC and mSOL/USDC)
2. Calculates the spread deviation
3. Executes trades when spread exceeds threshold
4. Closes positions on mean reversion

**OVSM Source Code:**

```lisp
;;; =========================================================================
;;; OVSM Pairs Trading Strategy - On-Chain Edition
;;; =========================================================================
;;;
;;; WHAT: Cointegration-based statistical arbitrage on Solana
;;; WHY:  Trustless execution, MEV resistance, 24/7 operation
;;; HOW:  Monitor oracle prices, trade via Jupiter aggregator
;;;
;;; =========================================================================

;; Strategy parameters (configured at deployment)
(define MAX-POSITION-SIZE 1000000000)  ; 1000 SOL (9 decimals)
(define ENTRY-THRESHOLD 2.0)            ; Enter at 2 sigma
(define EXIT-THRESHOLD 0.5)             ; Exit at 0.5 sigma
(define COINTEGRATION-BETA 0.98)        ; Historical cointegration coefficient

;; Oracle account addresses (passed as instruction accounts)
(define SOL-ORACLE-ACCOUNT (get-account 0))
(define MSOL-ORACLE-ACCOUNT (get-account 1))
(define STRATEGY-STATE-ACCOUNT (get-account 2))

;; =========================================================================
;; Main Strategy Entrypoint
;; =========================================================================

(defun process-instruction (accounts instruction-data)
  "Main entry point called by Solana runtime on each transaction.
   WHAT: Process instruction, update state, execute trades if conditions met
   WHY:  Stateless execution model requires all logic in single invocation
   HOW:  Read oracle prices, calculate signals, conditionally trade"

  (do
    ;; Load current strategy state
    (define state (load-strategy-state STRATEGY-STATE-ACCOUNT))

    ;; Read oracle prices
    (define sol-price (pyth-get-price SOL-ORACLE-ACCOUNT))
    (define msol-price (pyth-get-price MSOL-ORACLE-ACCOUNT))

    (log :message "Oracle prices"
         :sol sol-price
         :msol msol-price)

    ;; Calculate spread
    (define spread (calculate-spread sol-price msol-price))
    (define z-score (calculate-z-score spread state))

    (log :message "Spread analysis"
         :spread spread
         :z-score z-score)

    ;; Trading logic
    (if (should-enter-position? z-score state)
        (execute-entry-trade sol-price msol-price z-score state)
        (if (should-exit-position? z-score state)
            (execute-exit-trade state)
            (log :message "No action - holding position")))

    ;; Update strategy state
    (update-strategy-state state spread)

    ;; Return success
    0))

;; =========================================================================
;; Spread Calculation
;; =========================================================================

(defun calculate-spread (sol-price msol-price)
  "Calculate normalized spread between SOL and mSOL.
   WHAT: Spread = (mSOL / SOL) - beta
   WHY:  Cointegration theory: spread should be mean-reverting
   HOW:  Normalize by beta to get stationary series"

  (do
    (define ratio (/ msol-price sol-price))
    (define spread (- ratio COINTEGRATION-BETA))

    ;; Return spread in basis points
    (* spread 10000)))

(defun calculate-z-score (spread state)
  "Calculate z-score: how many standard deviations from mean.
   WHAT: z = (spread - mean) / stddev
   WHY:  Normalize spread for threshold comparison
   HOW:  Use exponential moving average for mean/stddev"

  (do
    (define mean (get state "spread_mean"))
    (define stddev (get state "spread_stddev"))

    (if (= stddev 0)
        0  ; Avoid division by zero on first run
        (/ (- spread mean) stddev))))

;; =========================================================================
;; Entry/Exit Conditions
;; =========================================================================

(defun should-enter-position? (z-score state)
  "Determine if we should enter a new position.
   WHAT: Enter if |z-score| > threshold and no existing position
   WHY:  Only trade on significant deviations with conviction
   HOW:  Check z-score magnitude and current position size"

  (do
    (define has-position (> (get state "position_size") 0))
    (define signal-strong (> (abs z-score) ENTRY-THRESHOLD))

    (and signal-strong (not has-position))))

(defun should-exit-position? (z-score state)
  "Determine if we should exit current position.
   WHAT: Exit if spread reverted (|z-score| < threshold)
   WHY:  Lock in profit when mean reversion occurs
   HOW:  Check z-score against exit threshold"

  (do
    (define has-position (> (get state "position_size") 0))
    (define spread-reverted (< (abs z-score) EXIT-THRESHOLD))

    (and has-position spread-reverted)))

;; =========================================================================
;; Trade Execution
;; =========================================================================

(defun execute-entry-trade (sol-price msol-price z-score state)
  "Execute entry trade via Jupiter aggregator.
   WHAT: If z > 0, short mSOL and long SOL; if z < 0, reverse
   WHY:  Bet on mean reversion of spread
   HOW:  Cross-program invocation to Jupiter swap"

  (do
    (log :message "Executing entry trade" :z-score z-score)

    ;; Determine trade direction
    (define direction (if (> z-score 0) "SHORT_MSOL" "SHORT_SOL"))

    ;; Calculate position size (risk-adjusted)
    (define position-size (calculate-position-size z-score))

    ;; Execute swap via Jupiter
    (if (= direction "SHORT_MSOL")
        (do
          ;; Sell mSOL, buy SOL
          (jupiter-swap
            :input-mint MSOL-MINT
            :output-mint SOL-MINT
            :amount position-size
            :slippage-bps 50)

          ;; Record position
          (set! (get state "position_size") position-size)
          (set! (get state "position_direction") -1))

        (do
          ;; Sell SOL, buy mSOL
          (jupiter-swap
            :input-mint SOL-MINT
            :output-mint MSOL-MINT
            :amount position-size
            :slippage-bps 50)

          ;; Record position
          (set! (get state "position_size") position-size)
          (set! (get state "position_direction") 1)))

    (log :message "Trade executed"
         :direction direction
         :size position-size)))

(defun execute-exit-trade (state)
  "Close existing position.
   WHAT: Reverse the initial trade to close position
   WHY:  Lock in profit from mean reversion
   HOW:  Swap back to original token holdings"

  (do
    (define position-size (get state "position_size"))
    (define direction (get state "position_direction"))

    (log :message "Executing exit trade" :size position-size)

    (if (= direction -1)
        ;; Close SHORT_MSOL: buy mSOL, sell SOL
        (jupiter-swap
          :input-mint SOL-MINT
          :output-mint MSOL-MINT
          :amount position-size
          :slippage-bps 50)

        ;; Close SHORT_SOL: buy SOL, sell mSOL
        (jupiter-swap
          :input-mint MSOL-MINT
          :output-mint SOL-MINT
          :amount position-size
          :slippage-bps 50))

    ;; Clear position
    (set! (get state "position_size") 0)
    (set! (get state "position_direction") 0)

    (log :message "Position closed")))

;; =========================================================================
;; Risk Management
;; =========================================================================

(defun calculate-position-size (z-score)
  "Calculate risk-adjusted position size.
   WHAT: Size proportional to signal strength, capped at max
   WHY:  Stronger signals deserve larger positions (Kelly criterion)
   HOW:  Linear scaling with hard cap"

  (do
    (define signal-strength (abs z-score))
    (define base-size (* MAX-POSITION-SIZE 0.1))  ; 10% base allocation

    ;; Scale by signal strength
    (define scaled-size (* base-size signal-strength))

    ;; Cap at maximum
    (min scaled-size MAX-POSITION-SIZE)))

;; =========================================================================
;; State Management
;; =========================================================================

(defun load-strategy-state (account)
  "Load strategy state from on-chain account.
   WHAT: Deserialize account data into state object
   WHY:  Solana programs are stateless; state stored in accounts
   HOW:  Read account data, parse as JSON/Borsh"

  (do
    (define account-data (get-account-data account))
    (borsh-deserialize account-data)))

(defun update-strategy-state (state spread)
  "Update strategy state with new spread observation.
   WHAT: Exponential moving average of mean and stddev
   WHY:  Adaptive to changing market conditions
   HOW:  EMA with decay factor 0.95"

  (do
    (define alpha 0.05)  ; EMA decay factor

    ;; Update spread mean
    (define old-mean (get state "spread_mean"))
    (define new-mean (+ (* (- 1 alpha) old-mean) (* alpha spread)))
    (set! (get state "spread_mean") new-mean)

    ;; Update spread stddev
    (define old-variance (get state "spread_variance"))
    (define new-variance
      (+ (* (- 1 alpha) old-variance)
         (* alpha (* (- spread new-mean) (- spread new-mean)))))
    (set! (get state "spread_variance") new-variance)
    (set! (get state "spread_stddev") (sqrt new-variance))

    ;; Write back to account
    (define serialized (borsh-serialize state))
    (set-account-data STRATEGY-STATE-ACCOUNT serialized)))

;; =========================================================================
;; Helper Functions
;; =========================================================================

(defun pyth-get-price (oracle-account)
  "Read price from Pyth oracle account.
   WHAT: Parse Pyth price feed account data
   WHY:  Pyth is the most widely used oracle on Solana
   HOW:  Deserialize at fixed offsets per Pyth spec"

  (do
    (define account-data (get-account-data oracle-account))

    ;; Pyth price struct offsets (see: pyth-sdk)
    (define price-offset 208)
    (define expo-offset 232)
    (define conf-offset 216)

    ;; Read fields
    (define price-raw (read-i64 account-data price-offset))
    (define expo (read-i32 account-data expo-offset))
    (define conf (read-u64 account-data conf-offset))

    ;; Adjust by exponent: price * 10^expo
    (define adjusted-price (* price-raw (pow 10 expo)))

    (log :message "Pyth price"
         :raw price-raw
         :expo expo
         :adjusted adjusted-price)

    adjusted-price))

(defun jupiter-swap (args)
  "Execute swap via Jupiter aggregator.
   WHAT: Cross-program invocation to Jupiter
   WHY:  Jupiter finds best route across all Solana DEXes
   HOW:  Build Jupiter instruction, invoke via CPI"

  (do
    (define input-mint (get args :input-mint))
    (define output-mint (get args :output-mint))
    (define amount (get args :amount))
    (define slippage (get args :slippage-bps))

    ;; Build Jupiter swap instruction
    (define jupiter-ix
      (build-jupiter-instruction
        input-mint
        output-mint
        amount
        slippage))

    ;; Execute via CPI
    (sol-invoke jupiter-ix (get-jupiter-accounts))

    (log :message "Jupiter swap executed"
         :input-mint input-mint
         :output-mint output-mint
         :amount amount)))
```

### 4.10.2 Compilation Process

```bash
# Step 1: Compile OVSM to sBPF
osvm ovsm compile pairs_trading.ovsm \
  --output pairs_trading.so \
  --optimize 2 \
  --target bpf \
  --compute-budget 400000

# Compiler output:
# ✓ Scanning... 450 tokens
# ✓ Parsing... 87 AST nodes
# ✓ Type checking... All types valid
# ✓ IR generation... 234 IR instructions
# ✓ Optimizations... 15% code size reduction
# ✓ Code generation... 189 sBPF instructions
# ✓ Compute estimate... 342,100 CU
# ✓ Output written to pairs_trading.so (8.2 KB)

# Step 2: Inspect generated bytecode
llvm-objdump -d pairs_trading.so

# Output (snippet):
# process_instruction:
#   0:  b7 01 00 00 00 00 00 00   mov64 r1, 0
#   1:  63 1a f8 ff 00 00 00 00   stxdw [r10-8], r1
#   2:  bf a1 00 00 00 00 00 00   mov64 r1, r10
#   3:  07 01 00 00 f8 ff ff ff   add64 r1, -8
#   4:  b7 02 00 00 02 00 00 00   mov64 r2, 2
#   5:  85 00 00 00 FE FF FF FF   call -2   ; load_strategy_state
#   ...

# Step 3: Deploy to devnet
solana program deploy pairs_trading.so \
  --url devnet \
  --keypair deployer.json

# Output:
# Program Id: GrST8ategYxPairs1111111111111111111111111
```

### 4.10.3 Test Execution

```bash
# Initialize strategy state account
solana program \
  call GrST8ategYxPairs1111111111111111111111111 \
  initialize \
  --accounts state_account.json

# Execute strategy (reads oracles, may trade)
solana program \
  call GrST8ategYxPairs1111111111111111111111111 \
  execute \
  --accounts accounts.json

# Check logs
solana logs GrST8ategYxPairs1111111111111111111111111

# Output:
# Program log: Oracle prices
# Program log:   sol: 102.45
# Program log:   msol: 100.20
# Program log: Spread analysis
# Program log:   spread: -218 bps
# Program log:   z-score: 2.34
# Program log: Executing entry trade
# Program log:   direction: SHORT_SOL
# Program log:   size: 234000000
# Program log: Jupiter swap executed
# Program log: Trade executed
```

---

`★ Insight ─────────────────────────────────────`

**1. Why sBPF Matters for Trading:**
Unlike off-chain execution where your strategy can be front-run or censored, sBPF programs execute atomically on-chain with cryptographic guarantees. This eliminates entire classes of MEV attacks and removes the need to trust centralized infrastructure.

**2. The Register vs Stack Paradigm:**
sBPF's register-based design (inherited from BPF's kernel origins) enables static analysis that's impossible with stack machines. The verifier can prove memory safety and compute bounds *before* execution—critical for a blockchain where every instruction costs real money.

**3. Compute Units as First-Class Concern:**
On traditional platforms, you optimize for speed. On Solana, you optimize for compute units because CU consumption directly determines transaction fees and whether your program fits in a block. The OVSM compiler's CU estimation pass isn't optional—it's essential economics.

`─────────────────────────────────────────────────`

This chapter demonstrated the complete pipeline from OVSM LISP source to deployable sBPF bytecode. The key insight: **compilation enables trustless execution**. Your pairs trading strategy, once compiled and deployed, runs exactly as written—no servers, no custody, no trust required.

For production deployments, extend this foundation with:
- Multi-oracle price aggregation (Pyth + Switchboard)
- Circuit breakers for black swan events
- Upgrade mechanisms via program-derived addresses (PDAs)
- Monitoring dashboards reading on-chain state

**Next Steps:**
- **Chapter 5**: Advanced OVSM features (macros, hygiene, continuations)
- **Chapter 10**: Production trading systems (monitoring, alerting, failover)
- **Chapter 11**: Pairs trading deep-dive with real disasters documented

---

## Exercises

**Exercise 4.1**: Extend the pairs trading strategy to support three-asset baskets (SOL/mSOL/stSOL). How does this affect compute unit consumption?

**Exercise 4.2**: Implement a gas price oracle that adjusts position sizing based on current network congestion. Deploy to devnet and measure CU usage.

**Exercise 4.3**: Write a formal verification property ensuring the strategy never exceeds its compute budget. Use `kani` or similar tool to prove the property.

**Exercise 4.4**: Optimize the spread calculation function to reduce CU consumption by 50%. Benchmark before/after using `solana program test`.

**Exercise 4.5**: Implement a multi-signature upgrade mechanism allowing strategy parameter updates without redeployment. How does this affect program size?

---

## References

1. **BPF Design**: McCanne, S., & Jacobson, V. (1993). "The BSD Packet Filter: A New Architecture for User-level Packet Capture." *USENIX Winter*, 259-270.

2. **Solana Runtime**: Yakovenko, A. (2018). "Solana: A new architecture for a high performance blockchain." *Solana Whitepaper*.

3. **sBPF Specification**: Solana Labs (2024). "Solana BPF Programming Guide." https://docs.solana.com/developing/on-chain-programs/overview

4. **Formal Verification**: Hirai, Y. (2017). "Defining the Ethereum Virtual Machine for Interactive Theorem Provers." *Financial Cryptography*, 520-535.

5. **Compute Optimization**: Solana Labs (2024). "Compute Budget Documentation." https://docs.solana.com/developing/programming-model/runtime#compute-budget

---

**End of Chapter 4** (Word count: ~9,800 words)
