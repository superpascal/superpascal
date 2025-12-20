# SuperPascal Language Specification — Semantics

## Semantic Rules and Language Meaning

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Semantic Analysis Overview

Semantic analysis determines the **meaning** of a program, beyond syntax:

- Name resolution (what does this identifier refer to?)
- Type checking (are types compatible?)
- Expression evaluation (what is the result type?)
- Control flow analysis (is code reachable?)
- Constant folding (can this be computed at compile time?)

---

## 2. Name Resolution and Scoping

### 2.1 Scope Rules

SuperPascal uses **lexical scoping** (static scoping):

1. **Local scope** (procedure/function body)
2. **Parameter scope** (procedure/function parameters)
3. **Unit-level scope** (unit interface + implementation)
4. **Imported unit scope** (units in `uses` clause)
5. **Global scope** (program-level)

**Resolution order**: Inner to outer (most specific first).

### 2.2 Identifier Resolution

When an identifier is encountered:

1. Search current scope
2. Search enclosing scopes (outward)
3. Search imported units (in `uses` order)
4. If not found: **error**

### 2.3 Shadowing

**Allowed**: Inner scope can shadow outer scope:

```pascal
var x: integer;  // Outer x

procedure Test;
var x: integer;  // Inner x shadows outer x
begin
  x := 5;  // Refers to inner x
end;
```

**Not allowed**: Same scope cannot redeclare:

```pascal
var x: integer;
var x: integer;  // ERROR: x already declared
```

### 2.4 Forward Declarations

Procedures/functions can be forward declared:

```pascal
procedure B; forward;

procedure A;
begin
  B;  // OK: B is forward declared
end;

procedure B;
begin
  // implementation
end;
```

---

## 3. Expression Semantics

### 3.1 Expression Evaluation Order

**Left-to-right evaluation** for:
- Function arguments
- Binary operators
- Array indexing
- Field access

**Example:**
```pascal
f(g(), h());  // g() evaluated before h()
a[i] + b[j];  // a[i] evaluated before b[j]
```

### 3.2 Short-Circuit Evaluation

**Logical operators** short-circuit:

- `A and B`: If `A` is false, `B` is not evaluated
- `A or B`: If `A` is true, `B` is not evaluated

**Example:**
```pascal
if (ptr <> nil) and (ptr^.Value > 0) then
  // Safe: ptr^.Value only evaluated if ptr <> nil
```

### 3.3 Constant Folding

Expressions with compile-time constants are evaluated at compile time:

```pascal
const X = 5 + 3;  // Folds to 8
const Y = X * 2;  // Folds to 16
```

**Foldable operations:**
- Arithmetic on constants
- Logical operations on constants
- Comparisons on constants
- String concatenation of literals

**Not foldable:**
- Operations involving variables
- Function calls (except built-in compile-time functions)

### 3.4 Expression Result Types

**Arithmetic:**
- `integer op integer` → `integer`
- `byte op byte` → `integer` (promoted)
- `word op word` → `word` or `integer` (depending on operation)

**Comparison:**
- Any comparable types → `boolean`

**Logical:**
- `boolean op boolean` → `boolean`

**String:**
- `string + string` → `string`
- `string op string` → `boolean` (comparison)

**Pointer Arithmetic (SuperPascal extension - BAREMETAL Mode Only):**

**Mode Restrictions:**
- **BAREMETAL mode:** ✅ Allowed
- **USER mode:** ❌ Forbidden (use OSPointer indexing instead)

**Operations (BAREMETAL mode only):**
- `^T + integer` → `^T` (pointer to T, scaled by sizeof(T))
- `^T - integer` → `^T` (pointer to T, scaled by sizeof(T))
- `^T - ^T` → `integer` (difference in elements, not bytes)
- Arithmetic is **type-aware**: scaling is automatic based on pointed-to type
- No bounds checking: accessing invalid memory is undefined behavior

**Pointer Arithmetic Examples (BAREMETAL mode):**
```pascal
{$EXECMODE BAREMETAL}

var p, q: ^integer;
var arr: array[0..9] of integer;
var offset: integer;

p := @arr[0];        // p points to arr[0] (address-of allowed in BAREMETAL)
p := p + 5;          // p now points to arr[5] (advances by 5 * 2 = 10 bytes)
q := p - 2;          // q points to arr[3] (decrements by 2 * 2 = 4 bytes)
offset := p - @arr[0]; // offset = 5 (element difference, not byte difference)
```

**Internal Use:**
- Pointer arithmetic is used internally by the compiler/runtime to implement OSPointer[T]
- User code in USER mode cannot directly use pointer arithmetic
- OSPointer indexing `p[i]` is implemented using pointer arithmetic behind the scenes

---

## 4. Assignment Semantics

### 4.1 Assignment Rules

Assignment `LHS := RHS` requires:

1. `LHS` is assignable (variable, field, array element, pointer dereference)
2. `RHS` type is compatible with `LHS` type
3. For subranges: value must fit in range (runtime check in debug)

### 4.2 Assignment Types

**Value assignment** (default):
- Copies value from RHS to LHS
- For records: copies all fields
- For arrays: copies all elements
- For strings: copies length + data

**Reference assignment** (classes, pointers):
- Copies reference (address)
- Does not copy object data

### 4.3 Multiple Assignment

Not directly supported (use multiple statements):

```pascal
x := 5;
y := 5;  // Not: x := y := 5;
```

---

## 5. Procedure and Function Call Semantics

### 5.1 Parameter Passing

**By value** (default):
- Parameter value is copied
- Modifications to parameter do not affect caller
- Used for input parameters

**By reference** (`var`):
- Parameter address is passed
- Modifications affect caller's variable
- Used for output/input-output parameters

**By constant reference** (`const`):
- Parameter address is passed (may be optimized)
- Modifications are not allowed
- Used for large input parameters (optimization)

### 5.2 Argument Evaluation

**Left-to-right evaluation**:

```pascal
procedure Test(a, b: integer);
begin
  // a evaluated before b
end;

Test(f(), g());  // f() called before g()
```

### 5.3 Return Values

**Functions:**
- Return value via `Result` variable
- `Result` type must match function return type
- `Result` is implicitly declared

**Procedures:**
- No return value
- Use `var` parameters for output

---

## 6. Method Dispatch

### 6.1 Non-Virtual Methods

**Resolution**: Compile time

```pascal
var obj: TEntity;
obj.NonVirtualMethod;  // Direct call to TEntity.NonVirtualMethod
```

**Rules:**
- Method address known at compile time
- No runtime lookup
- Fastest dispatch

### 6.2 Virtual Methods

**Resolution**: Runtime (via vtable)

```pascal
var obj: TObject;
obj := TEntity.Create;
obj.VirtualMethod;  // Calls TEntity.VirtualMethod via vtable
```

**Rules:**
- Method slot index determined at compile time
- Actual method address from vtable at runtime
- Supports polymorphism

### 6.3 Method Overriding

**Rules:**
- `override` method must match parent's `virtual` method signature
- Overridden method replaces parent's vtable slot
- Cannot override non-virtual method

---

## 7. Control Flow Semantics

### 7.1 If Statement

**Semantics:**
1. Evaluate condition
2. If true: execute `then` branch
3. If false: execute `else` branch (if present)
4. Continue after statement

**Type requirement**: Condition must be `boolean`.

### 7.2 While Statement

**Semantics:**
1. Evaluate condition
2. If true: execute body, then repeat from step 1
3. If false: continue after statement

**Type requirement**: Condition must be `boolean`.

### 7.3 Repeat Statement

**Semantics:**
1. Execute body
2. Evaluate condition
3. If false: repeat from step 1
4. If true: continue after statement

**Note**: Body executes at least once.

### 7.4 For Statement

**Semantics:**
1. Evaluate initial value
2. Evaluate final value
3. Assign initial to loop variable
4. If direction is `to` and variable ≤ final: execute body, increment, repeat
5. If direction is `downto` and variable ≥ final: execute body, decrement, repeat
6. Continue after statement

**Rules:**
- Loop variable is read-only in body
- Initial and final evaluated once
- Loop variable must be ordinal type

### 7.5 Case Statement

**Semantics:**
1. Evaluate case expression
2. Find matching label
3. Execute corresponding statement
4. Continue after `end`

**Rules:**
- Expression must be ordinal type
- Labels must be compile-time constants
- Labels must be unique
- `else` clause executes if no match

---

## 8. Exception Semantics

### 8.1 Try-Except

**Semantics:**
1. Push exception frame
2. Execute `try` block
3. If exception raised: pop frame, execute `except` block
4. If no exception: pop frame, skip `except` block
5. Continue after statement

### 8.2 Try-Finally

**Semantics:**
1. Push exception frame
2. Execute `try` block
3. Always execute `finally` block (even if exception)
4. Pop frame
5. If exception was active: re-raise
6. Continue after statement

### 8.3 Raise

**Semantics:**
1. Unwind stack to nearest exception frame
2. Restore stack pointer and frame pointer
3. Jump to exception handler
4. If no frame: abort program

---

## 9. Concurrency Semantics

### 9.1 Parallel Statement Semantics

**Semantics:**
1. All processes in `parallel` block are activated
2. Each process gets its own stack frame
3. Processes execute cooperatively (cooperative multitasking)
4. Execution continues after `endparallel` only when all processes complete
5. If all processes block (deadlock), runtime error is raised

**Process Activation:**
- Each process gets its own activation record
- Activation record contains: Program Counter, Block Pointer, Link
- Processes are added to ready queue
- Scheduler selects next ready process (FIFO by default)

**Context Switching:**
- Occurs at blocking operations (send/receive)
- Occurs when process completes
- Minimal overhead: only 3 words saved/restored per process (on retro platforms)

**Example:**
```pascal
parallel
  process
    ComputeA;  // Process 1
  endprocess |
  process
    ComputeB;  // Process 2
  endprocess
endparallel
// Execution continues here only after both processes complete
```

**Deadlock Detection:**
- If all processes are blocked and ready queue is empty → deadlock
- Runtime raises deadlock error with line number information

### 9.2 Forall Statement Semantics

**Semantics:**
1. Evaluate loop bounds (once)
2. Create one process per iteration value
3. Each process executes statement body with its loop variable value
4. All processes execute concurrently
5. Execution continues after `forall` only when all processes complete

**Process Creation:**
- Number of processes = `upper - lower + 1`
- Each process gets its own copy of loop variable
- All processes share same statement body (code)

**Example:**
```pascal
forall i := 1 to 10 do
  Process(i);  // Creates 10 processes, each with different i value
```

**Rules:**
- Loop variable is read-only in body
- Initial and final expressions evaluated once
- All processes must complete before continuing
- Processes cannot share mutable state (compile-time checked)

### 9.3 Channel Communication Semantics

#### 9.3.1 Synchronous Channels

**Send Semantics:**
1. Check channel is open
2. Search for matching receiver on same channel
3. If receiver found: copy message data, wake receiver, continue
4. If no receiver: block current process, add to channel wait queue, context switch

**Receive Semantics:**
1. Check channel is open
2. Search for matching sender on same channel
3. If sender found: copy message data, wake sender, continue
4. If no sender: block current process, add to channel wait queue, context switch

**Rendezvous:**
- Both sender and receiver must be ready
- Direct stack-to-stack copy (zero-copy)
- Both processes continue after rendezvous
- Deterministic execution order

**Example:**
```pascal
type IntChannel = channel[integer];
var c: IntChannel;
var value: integer;

open(c);

parallel
  process
    send(c, 42);  // Blocks until receiver ready
  endprocess |
  process
    receive(c, value);  // Blocks until sender ready
  endprocess
endparallel
// Both processes continue after rendezvous
```

#### 9.3.2 Asynchronous Channels

**Send Semantics:**
1. Check channel is open
2. If buffer space available: add message to buffer, continue
3. If buffer full: block current process, add to wait queue, context switch

**Receive Semantics:**
1. Check channel is open
2. If buffer not empty: remove message from buffer, continue
3. If buffer empty: block current process, add to wait queue, context switch

**Buffering:**
- Messages stored in heap-allocated buffer
- FIFO ordering (first in, first out)
- Non-deterministic execution (depends on scheduling)

**Example:**
```pascal
type AsyncChannel = async channel[integer][10];
var c: AsyncChannel;
var value: integer;

open(c);

parallel
  process
    send(c, 1);  // Non-blocking if buffer space available
    send(c, 2);
    send(c, 3);
  endprocess |
  process
    receive(c, value);  // Blocks if buffer empty
    WriteLn(value);
  endprocess
endparallel
```

### 9.4 Disjointness Checking (Compile-Time)

**Purpose:** Prevent data races by ensuring parallel processes don't write to same variables.

**Rules:**
1. Track all variables **written** (target variables) in each process
2. Track all variables **read** (expression variables) in each process
3. Check that target variable sets are **disjoint** across all processes
4. If overlap found: compile-time error

**Allowed:**
- Multiple processes can **read** same variable (no interference)
- Processes can read variables written by other processes (after synchronization)

**Not Allowed:**
- Multiple processes **writing** to same variable (data race)
- Process writing variable that another process reads without synchronization

**Example - Error:**
```pascal
var x, y: integer;

parallel
  process
    x := 1;  // ERROR: writes to x
  endprocess |
  process
    x := 2;  // ERROR: writes to x (overlap with process 1)
  endprocess
endparallel
```

**Example - Allowed:**
```pascal
var x, y: integer;

parallel
  process
    x := 1;  // OK: only process 1 writes to x
  endprocess |
  process
    y := x;  // OK: process 2 reads x (no write conflict)
  endprocess
endparallel
```

**Implementation:**
- Performed during semantic analysis
- Builds variable usage sets for each process
- Compares sets for intersection
- Reports errors with process and variable information

**Channel Synchronization:**
- Variables written before `send` are visible to receiver after `receive`
- Channel operations provide synchronization points
- Disjointness checking accounts for channel synchronization

---

## 10. Memory Model

### 10.1 Storage Classes

**Static** (global variables):
- Allocated at program start
- Lifetime: entire program execution
- Initialized to zero (or constant value)

**Automatic** (local variables):
- Allocated on stack (frame)
- Lifetime: procedure/function execution
- Not initialized (undefined until assigned)

**Heap** (dynamic allocation):
- Allocated via `New` or `GetMem`
- Lifetime: until `Dispose` or `FreeMem`
- Not initialized (undefined until assigned)

### 10.2 Memory Layout

**Stack** (grows downward):
- Function frames
- Local variables
- Parameters
- Return addresses

**Heap** (managed by runtime):
- Class instances
- Dynamically allocated records
- Strings (if allocated)

**Static** (fixed addresses):
- Global variables
- Constants
- Code

### 10.3 Lifetime Rules

**Variables:**
- Scope determines visibility
- Storage class determines lifetime
- Out-of-scope variables are inaccessible

**Objects:**
- Created with `New` or constructor
- Destroyed with `Dispose` or destructor
- References can outlive objects (dangling pointer risk)

### 10.4 Concurrent Memory Model

**Single Address Space:**
- All processes share same global memory
- Processes have separate stack frames
- No memory protection between processes
- Compile-time interference checking prevents data races

**Stack Frame Isolation:**
- Each process has its own stack frame
- Local variables are isolated per process
- Global variables are shared (read-only access allowed, write access checked)

**Channel Communication:**
- Messages copied between process stacks
- Synchronous: direct stack-to-stack copy (zero-copy)
- Asynchronous: heap-allocated buffer

**Memory Safety:**
- Compile-time: Disjointness checking prevents data races
- Runtime: Stack overflow detection (optional, debug mode)
- No automatic garbage collection (manual memory management)

### 10.5 Capability-Based Memory Model (OSPointer)

**Overview:**
In USER mode, SuperPascal uses a capability-based memory model via OSPointer[T] types. This provides memory safety without hardware MMU support.

**OSPointer Semantics:**

**Construction:**
- OSPointers can only be obtained via OS system calls
- Cannot be created from raw addresses or integers
- Cannot be cast from raw pointers
- OS validates and tracks all OSPointers

**Access Semantics:**
- **Indexing**: `p[i]` is the only access method
- **Bounds checking**: Every access checks `0 <= i < length`
- **Permission checking**: Write requires WRITE permission, read requires READ permission
- **Type checking**: Element type must match OSPointer type parameter
- **Runtime validation**: OS validates capability authenticity on each access

**Indexing Semantics:**
```pascal
var p: OSPointer[Integer];
var value: Integer;

value := p[5];  // Read: checks bounds, permissions, validates capability
p[5] := 42;     // Write: checks bounds, permissions, validates capability
```

**Implementation:**
- Compiler generates checked access code for `p[i]`
- Uses pointer arithmetic internally but not exposed to user
- OS mediates all memory accesses through capability system
- Bounds and permissions checked at runtime

**Error Handling:**
- Out-of-bounds access: Runtime error (process termination or exception)
- Invalid capability: Runtime error (use-after-free detection)
- Permission violation: Runtime error (write to read-only)

**Lifetime:**
- OSPointers are tied to OS-managed memory regions
- Must be explicitly freed via OS calls
- Use-after-free detected via capability validation
- Memory zeroed on allocation and free

**Platform-Specific:**
- **65C816 (PascalOS)**: Bank-based (256 banks of 64KB)
- **Other platforms**: Page-based or region-based
- Implementation details vary but interface is consistent

---

## 11. Constant Semantics

### 11.1 Constant Evaluation

Constants must be evaluable at compile time:

```pascal
const
  X = 5 + 3;        // OK: compile-time constant
  Y = X * 2;        // OK: folds to 16
  // Z = ReadInt;   // ERROR: not compile-time
```

### 11.2 Constant Types

Constants infer type from value:

```pascal
const
  I = 42;        // integer
  C = 'A';      // char
  S = 'Hello';  // string
  B = true;     // boolean
```

### 11.3 Typed Constants

Constants can have explicit type:

```pascal
const
  X: integer = 42;
  Y: string = 'Hello';
```

---

## 12. Unit Semantics

### 12.1 Unit Initialization

Units can have initialization code:

```pascal
unit MyUnit;
implementation
begin
  // Initialization code
  // Runs at program start
end.
```

**Order:**
1. Initialize units in dependency order
2. Run unit initialization blocks
3. Run program main block

### 12.2 Unit Finalization

Not supported in initial version (future feature).

### 12.3 Interface vs Implementation

**Interface:**
- Exported declarations (visible to other units)
- Types, constants, procedures, functions
- No implementation

**Implementation:**
- Private declarations (not exported)
- Implementations of interface declarations
- Additional private helpers

---

## 13. Error Conditions

### 13.1 Compile-Time Errors

- Syntax errors
- Type errors
- Undefined identifiers
- Duplicate declarations
- Invalid operations

### 13.2 Runtime Errors (Debug Mode)

- Array index out of bounds
- Subrange value out of range
- Nil pointer dereference
- Division by zero
- String length overflow

### 13.3 Undefined Behavior

- Accessing uninitialized variables
- Dereferencing invalid pointers
- Modifying constants (if somehow possible)
- Stack overflow

---

## 14. Semantic Analysis Summary

**Key Phases:**
1. **Name resolution**: Resolve all identifiers
2. **Type checking**: Verify type compatibility
3. **Constant folding**: Evaluate compile-time constants
4. **Control flow**: Verify reachability and returns
5. **Error reporting**: Collect and report semantic errors

**Output:**
- Annotated AST (with types and symbols)
- List of semantic errors
- Symbol table
- Type information for code generation

---

**End of Semantics Specification**

