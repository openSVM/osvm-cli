# OVSM V6.1 - Async/Await Guide

## Overview

OVSM V6.1 introduces JavaScript-style async/await for concurrent task execution. Tasks run in a thread pool (powered by Rayon), allowing non-blocking concurrent operations.

## Quick Start

```lisp
;; Define an async function
(defun compute (x y)
  (* x y))

;; Launch async task - returns immediately with handle
(define handle (async compute 5 7))

;; Do other work...

;; Await result - blocks until task completes
(define result (await handle))  ;; => 35
```

## Core Concepts

### 1. `async` Function

Launches a function in the thread pool and returns an `AsyncHandle`.

```lisp
(async function-name arg1 arg2 ...)  ;; Returns <async-handle:async_N>
```

**Key Points:**
- Returns immediately (non-blocking)
- Function executes in isolated evaluator (see limitations below)
- Handle can be awaited or ignored (fire-and-forget)

### 2. `await` Function

Blocks current thread until async task completes and returns result.

```lisp
(define result (await handle))
```

**Key Points:**
- Blocks until task finishes
- Returns the task's result value
- Handle can only be awaited once
- If task panicked, returns error

### 3. Fire-and-Forget Pattern

Launch tasks without waiting for results:

```lisp
(async background-task arg1 arg2)  ;; No await - task runs in background
```

## Common Patterns

### Pattern 1: Concurrent Batch Processing

Process multiple items concurrently:

```lisp
(defun process-item (item)
  (do
    (sleep 50)  ;; Simulate I/O
    (* item 2)))

(define items [1 2 3 4 5])

;; Launch all tasks
(define handles [])
(for (item items)
  (set! handles (append handles [(async process-item item)])))

;; Collect results
(define results [])
(for (h handles)
  (set! results (append results [(await h)])))

;; results => [2, 4, 6, 8, 10]
```

### Pattern 2: Map-Reduce

Parallel map phase, sequential reduce:

```lisp
(defun mapper (chunk)
  (do
    (define sum 0)
    (for (val chunk)
      (set! sum (+ sum val)))
    sum))

(define chunks [[1 2 3] [4 5 6] [7 8 9]])

;; Map phase (parallel)
(define handles [])
(for (chunk chunks)
  (set! handles (append handles [(async mapper chunk)])))

(define sums [])
(for (h handles)
  (set! sums (append sums [(await h)])))

;; Reduce phase (sequential)
(define total 0)
(for (s sums)
  (set! total (+ total s)))

;; total => 45
```

### Pattern 3: Pipeline Processing

Multi-stage processing with concurrency at each stage:

```lisp
(defun stage1 (id) {:id id :data (str "raw-" id)})
(defun stage2 (item) {:id (get item :id) :data (str "processed-" (get item :data))})

(define ids [1 2 3])

;; Stage 1 (parallel)
(define s1-handles [])
(for (id ids)
  (set! s1-handles (append s1-handles [(async stage1 id)])))

(define s1-results [])
(for (h s1-handles)
  (set! s1-results (append s1-results [(await h)])))

;; Stage 2 (parallel)
(define s2-handles [])
(for (item s1-results)
  (set! s2-handles (append s2-handles [(async stage2 item)])))

(define final-results [])
(for (h s2-handles)
  (set! final-results (append final-results [(await h)])))
```

### Pattern 4: Fan-Out/Fan-In

Distribute work to workers, collect results:

```lisp
(defun worker (worker-id tasks)
  (do
    (sleep (* worker-id 10))
    {:worker-id worker-id :completed (length tasks)}))

(define workers 4)
(define tasks-per-worker 25)

;; Fan-out
(define handles [])
(for (i (range 1 (+ workers 1)))
  (set! handles (append handles [(async worker i tasks-per-worker)])))

;; Fan-in
(define results [])
(for (h handles)
  (set! results (append results [(await h)])))
```

## Limitations & Known Issues

### Isolated Evaluator

Each async task runs in a **completely isolated** evaluator. This means:

❌ **Cannot Access:**
- Global variables from parent scope
- Functions defined in parent scope (except passed as lambdas)
- Closures over parent variables

✅ **CAN Access:**
- Function parameters
- Variables defined inside async function
- Helper functions defined inside async function

**Example - WRONG:**

```lisp
(define global-var 10)

(defun bad-async ()
  (* global-var 2))  ;; ERROR: Undefined variable: global-var

(await (async bad-async))
```

**Example - CORRECT:**

```lisp
(define global-var 10)

(defun good-async (param)
  (* param 2))

(await (async good-async global-var))  ;; Pass as parameter
```

### Self-Contained Async Functions

For complex async functions, define all helpers inside:

```lisp
(defun process-async (data)
  (do
    ;; Helper function INSIDE async function
    (defun helper (x)
      (* x 2))

    ;; Now helper is available
    (helper data)))
```

## Performance Characteristics

### Thread Pool

- Uses Rayon thread pool
- Default: `num_cpus` worker threads
- Tasks scheduled dynamically

### Overhead

- **Task Creation**: ~1-5μs per task
- **Context Switch**: Minimal (Rayon work-stealing)
- **Memory**: Each task ~4KB stack + closure data

### Best Practices

1. **Use for I/O-bound operations**: Network calls, file I/O, sleep
2. **Batch small tasks**: Don't create thousands of tiny tasks
3. **Prefer parallelism over concurrency**: For CPU-bound work, use parallel map
4. **Limit concurrency**: Cap max concurrent tasks to avoid overwhelming resources

### Performance Comparison

| Operation | Sequential (100 tasks) | Concurrent (100 tasks) | Speedup |
|-----------|------------------------|------------------------|---------|
| I/O-bound (sleep 50ms) | ~5000ms | ~50-100ms | ~50-100x |
| CPU-bound (factorial) | ~10ms | ~2-5ms | ~2-5x |
| Task creation overhead | 0ms | ~0.5ms | N/A |

## Examples

### Full Examples

- `examples/ovsm_scripts/async_basics.ovsm` - Basic async/await patterns
- `examples/ovsm_scripts/async_batch_processing.ovsm` - Real-world patterns
- `examples/ovsm_scripts/async_wallet_discovery.ovsm` - Blockchain use case
- `/tmp/wallet_discovery_depth10_limited.ovsm` - 10-hop concurrent traversal

### Running Examples

```bash
# Basic examples
osvm ovsm run examples/ovsm_scripts/async_basics.ovsm

# Batch processing patterns
osvm ovsm run examples/ovsm_scripts/async_batch_processing.ovsm

# Wallet discovery (blockchain)
osvm ovsm run examples/ovsm_scripts/async_wallet_discovery.ovsm

# Run benchmarks
cd crates/ovsm
cargo bench --bench async_bench
```

## Benchmarks

Run performance benchmarks:

```bash
cd crates/ovsm
cargo bench --bench async_bench
```

Available benchmarks:
- `sequential_vs_concurrent` - Compare performance at different scales
- `async_overhead` - Measure task creation and await costs
- `batch_processing` - Batch processing performance
- `map_reduce` - Map-reduce pattern performance
- `factorial_concurrent` - CPU-intensive concurrent workload
- `fire_and_forget` - Fire-and-forget pattern overhead

## Troubleshooting

### "Undefined variable" in async function

**Problem**: Cannot access parent scope variables

**Solution**: Pass variables as parameters or define inside async function

```lisp
;; BAD
(define x 10)
(defun bad () (* x 2))
(await (async bad))  ;; ERROR

;; GOOD
(define x 10)
(defun good (param) (* param 2))
(await (async good x))  ;; OK
```

### "Undefined tool" in async function

**Problem**: Cannot call parent scope functions

**Solution**: Define function inside async function or pass as lambda

```lisp
;; BAD
(defun helper (x) (* x 2))
(defun bad (y) (helper y))
(await (async bad 5))  ;; ERROR

;; GOOD
(defun good (y)
  (do
    (defun helper (x) (* x 2))
    (helper y)))
(await (async good 5))  ;; OK
```

### Task taking too long

**Problem**: Blocking operation in async task

**Solution**: Ensure tasks are truly concurrent, not waiting on each other

### Too many tasks

**Problem**: Creating thousands of tasks overwhelms thread pool

**Solution**: Batch tasks or limit concurrency

```lisp
;; Limit to 100 concurrent tasks at a time
(define max-concurrent 100)
(define items (range 1 1001))

(define results [])
(define i 0)
(while (< i (length items))
  (do
    (define batch (take max-concurrent (drop i items)))
    (define handles [])
    (for (item batch)
      (set! handles (append handles [(async process item)])))
    (for (h handles)
      (set! results (append results [(await h)])))
    (set! i (+ i max-concurrent))))
```

## Version History

- **V6.1** (Nov 2024): Initial async/await implementation
  - `async` function returns AsyncHandle
  - `await` function blocks for result
  - Rayon thread pool backend
  - Isolated evaluator per task

## API Reference

### Functions

#### `(async function ...args)`

Launch async task.

**Returns**: `AsyncHandle` value

**Example**:
```lisp
(define h (async my-function arg1 arg2))
```

#### `(await handle)`

Wait for async task to complete.

**Parameters**:
- `handle`: AsyncHandle returned from `async`

**Returns**: Result value from async task

**Example**:
```lisp
(define result (await h))
```

### Value Types

#### AsyncHandle

Opaque handle to async task.

**String representation**: `<async-handle:async_N>`

**Properties**:
- Can only be awaited once
- Becomes invalid after await
- Contains unique task ID

## Further Reading

- [OVSM README](README.md) - General OVSM documentation
- [USAGE_GUIDE](USAGE_GUIDE.md) - Getting started with OVSM
- [BUILTIN_FUNCTIONS](BUILTIN_FUNCTIONS.md) - All built-in functions

## Contributing

Found a bug or have a feature request? Open an issue on GitHub!

Performance improvements and new examples are always welcome.
