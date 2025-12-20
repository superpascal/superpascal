# SuperPascal Language Specification — Capability-Based Memory Model

## OSPointer and Memory Safety Without MMU

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Overview

SuperPascal provides a **capability-based memory model** via `OSPointer[T]` types for platforms without hardware Memory Management Units (MMUs). This enables memory safety and process isolation on retro platforms like the 65C816 processor.

**Key Concepts:**
- **OSPointer[T]**: Opaque capability type for safe memory access
- **Two-tier model**: Bare metal (raw pointers) vs User mode (capability pointers)
- **Runtime checking**: Bounds and permissions validated on every access
- **OS mediation**: All dynamic memory managed through OS system calls

---

## 2. Design Goals

### 2.1 Safety Without Hardware Support

**Challenge:** Retro platforms (65C816, Z80, 6502) lack MMUs for hardware-enforced memory protection.

**Solution:** Software-enforced memory safety through:
- Compile-time restrictions (no raw pointers in user mode)
- Runtime bounds checking (every OSPointer access)
- Capability validation (prevent forgery and use-after-free)
- OS-managed memory regions (bank/page isolation)

### 2.2 Educational Clarity

**Goal:** Make memory safety mechanisms transparent and understandable.

**Approach:**
- Clear mode distinction (`{$EXECMODE BAREMETAL}` vs `{$EXECMODE USER}`)
- Explicit OS calls for memory management
- Predictable error behavior (no silent corruption)
- Inspectable runtime checks

### 2.3 Performance Considerations

**Balance:** Safety vs performance on resource-constrained platforms.

**Strategy:**
- Minimal overhead for common cases (indexed access)
- Efficient capability representation (small handles)
- Bank-based allocation (reduce fragmentation)
- Zero-on-alloc/free (security and debugging)

---

## 3. Two-Tier Pointer Model

### 3.1 Bare Metal Mode

**Purpose:** Low-level system programming, device drivers, OS kernel.

**Features:**
- ✅ Raw pointers (`^T`)
- ✅ Pointer arithmetic
- ✅ Address-of operator (`@`)
- ✅ `Ptr(addr)` function
- ✅ Type casting to/from integers
- ❌ No runtime bounds checking
- ❌ No OSPointer (OS feature)

**Use Cases:**
- Operating system kernel
- Device drivers
- Bootloaders
- Bare metal applications (no OS)

**Example:**
```pascal
{$EXECMODE BAREMETAL}

var p: ^Integer;
var addr: LongInt;

p := Ptr($00FF1234);  // Create pointer from address
p := p + 5;           // Pointer arithmetic
p^ := 42;             // Direct dereference
```

### 3.2 User Mode (PascalOS)

**Purpose:** Application programs running under an OS.

**Features:**
- ❌ Raw pointers (`^T`) forbidden
- ❌ Pointer arithmetic forbidden (except internally)
- ❌ Address-of operator (`@`) forbidden (except internally)
- ✅ OSPointer[T] required for dynamic memory
- ✅ Array indexing `p[i]` (checked)
- ✅ Runtime bounds checking
- ✅ Permission checking

**Use Cases:**
- User applications
- Games
- Educational programs
- Any code running under PascalOS

**Example:**
```pascal
{$EXECMODE USER}

var buf: OSPointer[Byte];
var value: Byte;

buf := BankAlloc(1024);  // Get from OS
value := buf[5];          // ✅ Checked access
buf[5] := 42;            // ✅ Checked write
// buf^ := 42;           // ❌ Forbidden
// buf := buf + 5;       // ❌ Forbidden
```

---

## 4. OSPointer Type System

### 4.1 Type Definition

**Syntax:**
```pascal
type
  IntBuffer = OSPointer[integer];
  ByteBuffer = OSPointer[byte];
  StringBuffer = OSPointer[char];
```

**Properties:**
- **Generic type**: Parameter specifies element type
- **Opaque**: Internal structure not accessible
- **Nominal**: Two OSPointers with same element type are compatible
- **Non-castable**: Cannot cast to/from other types

### 4.2 Internal Structure (Implementation Detail)

**Capability Record:**
```pascal
// Internal structure (not accessible to user code)
type
  OSPointerInternal = record
    BankID: Byte;      // Bank/page identifier
    Base: Word;        // Offset within bank
    Length: Word;      // Size of region
    Rights: Byte;      // READ, WRITE, EXEC, SHARE
    Tag: Word;         // Validation token
  end;
```

**Platform-Specific:**
- **65C816**: Bank-based (256 banks of 64KB)
- **Z80**: Page-based (varies by MMU)
- **Other**: Region-based or page-based

### 4.3 Access Methods

**Array Indexing (Only Method):**
```pascal
var p: OSPointer[Integer];
var value: Integer;

value := p[5];   // Read: checks bounds, permissions, validates
p[5] := 42;      // Write: checks bounds, permissions, validates
```

**Indexing Semantics:**
- Zero-based indexing
- Type-aware scaling (index * sizeof(T))
- Bounds checked: `0 <= index < length`
- Permission checked: READ for read, WRITE for write
- Capability validated: OS verifies pointer authenticity

**Forbidden Operations:**
- `p^` - No dereference syntax
- `p + n` - No pointer arithmetic
- `@p` - Cannot take address
- Casting - Cannot cast to/from other types

---

## 5. Memory Management API

### 5.1 Allocation

**BankAlloc:**
```pascal
function BankAlloc(numBytes: Cardinal): OSPointer[Byte];
```

**Semantics:**
- Allocates memory region of requested size
- Returns OSPointer[Byte] capability
- Memory is zero-initialized
- Returns nil-capability on failure
- Rounds up to bank/page boundaries (platform-specific)

**Example:**
```pascal
var buf: OSPointer[Byte];
buf := BankAlloc(1024);
if buf.Length > 0 then
  // Use buffer
```

### 5.2 Deallocation

**BankFree:**
```pascal
function BankFree(ptr: OSPointer[Byte]): Boolean;
```

**Semantics:**
- Frees previously allocated memory
- Validates pointer ownership
- Zeros memory on free (security)
- Marks capability as invalid
- Returns true on success, false on failure

**Example:**
```pascal
if BankFree(buf) then
  WriteLn('Freed successfully')
else
  WriteLn('Free failed');
```

### 5.3 Sharing

**BankGrant:**
```pascal
function BankGrant(ptr: OSPointer[Byte]; targetPID: PID; rights: Byte): Boolean;
```

**Semantics:**
- Grants access to memory region to another process
- Creates new capability for target process
- Rights must be subset of granter's rights
- Returns true on success

**BankRevoke:**
```pascal
function BankRevoke(ptr: OSPointer[Byte]; targetPID: PID): Boolean;
```

**Semantics:**
- Revokes previously granted access
- Invalidates target process's capability
- Returns true on success

---

## 6. Runtime Safety Mechanisms

### 6.1 Bounds Checking

**Every Access:**
```pascal
value := p[i];  // Checks: 0 <= i < p.Length
```

**Implementation:**
- Compiler generates bounds check code
- Check performed before memory access
- Out-of-bounds: Runtime error (process termination or exception)

**Performance:**
- Minimal overhead (single comparison)
- Can be optimized away in some cases (compile-time known bounds)
- Required for safety (cannot be disabled in user mode)

### 6.2 Permission Checking

**Read Access:**
- Checks READ permission in capability
- No permission: Runtime error

**Write Access:**
- Checks WRITE permission in capability
- No permission: Runtime error

**Implementation:**
- Permission bits checked at runtime
- OS validates permissions on syscall
- Compiler may optimize away redundant checks

### 6.3 Capability Validation

**Purpose:** Prevent forgery and use-after-free.

**Mechanisms:**
- **Tag validation**: Each capability has unique tag
- **Generation counter**: Incremented on free
- **Table lookup**: OS maintains table of valid capabilities
- **Ownership check**: Verifies process owns capability

**On Access:**
- OS validates capability authenticity
- Checks tag/generation matches
- Verifies ownership
- Invalid capability: Runtime error

### 6.4 Zero-on-Alloc/Free

**Security Measure:**
- All allocated memory zeroed before use
- All freed memory zeroed on release
- Prevents information leakage
- Aids debugging (use-after-free obvious)

**Implementation:**
- OS performs zeroing efficiently (bulk operations)
- May use DMA or optimized loops
- Required for security (cannot be disabled)

---

## 7. Compiler Implementation

### 7.1 OSPointer Indexing Lowering

**Source Code:**
```pascal
var p: OSPointer[Integer];
var value: Integer;
value := p[5];
p[5] := 42;
```

**Lowered Code (Pseudocode):**
```pascal
// Read
if 5 >= p.Length then
  RuntimeError('Out of bounds');
if not (p.Rights and READ) then
  RuntimeError('No read permission');
OS.ValidateCapability(p);
value := MemRead32(p.BankID, p.Base + 5 * sizeof(Integer));

// Write
if 5 >= p.Length then
  RuntimeError('Out of bounds');
if not (p.Rights and WRITE) then
  RuntimeError('No write permission');
OS.ValidateCapability(p);
MemWrite32(p.BankID, p.Base + 5 * sizeof(Integer), 42);
```

### 7.2 Internal Pointer Arithmetic

**Implementation Detail:**
- OSPointer indexing uses pointer arithmetic internally
- Compiler generates: `base + index * sizeof(T)`
- User code cannot access raw pointer operations
- All arithmetic is bounds-checked

**Example:**
```pascal
// User code
value := p[i];

// Internal implementation (not accessible)
var rawPtr: ^Integer;
rawPtr := Ptr(CalculateAddress(p, i));  // Internal only
value := rawPtr^;  // Internal only
```

### 7.3 Mode Enforcement

**Compile-Time:**
- Parser checks mode directive
- Type checker enforces mode restrictions
- Code generator respects mode rules

**Runtime:**
- OS validates all syscalls
- Mode cannot be changed at runtime
- Violations result in process termination

---

## 8. Platform-Specific Implementations

### 8.1 65C816 (PascalOS)

**Memory Model:**
- 24-bit address space (16MB)
- 256 banks of 64KB each
- Bank registers (DBR, PBR) for isolation
- No hardware MMU

**OSPointer Implementation:**
- Bank ID: 8-bit (0-255)
- Base offset: 16-bit (0-65535)
- Length: 16-bit (bytes or elements)
- Rights: 8-bit (READ, WRITE, EXEC, SHARE)

**Bank Isolation:**
- Each process assigned one or more banks
- DBR set to process's data bank on context switch
- Long addressing only via OS calls
- Compiler avoids generating long addresses in user mode

### 8.2 Z80 (ZealZ80)

**Memory Model:**
- 16-bit address space (64KB)
- MMU provides page mapping
- Pages: 4KB or 8KB (platform-specific)

**OSPointer Implementation:**
- Page ID: Platform-specific
- Base offset: Within page
- Length: Page-aligned
- Rights: Same as 65C816

### 8.3 Other Platforms

**Generic Model:**
- Region-based allocation
- Platform-specific page/bank size
- Same OSPointer interface
- Implementation details vary

---

## 9. Error Handling

### 9.1 Runtime Errors

**Out-of-Bounds:**
- Access with invalid index
- Response: Process termination or exception
- Message: "Memory access out of bounds"

**Invalid Capability:**
- Use-after-free
- Forged capability
- Response: Process termination
- Message: "Invalid memory capability"

**Permission Violation:**
- Write to read-only
- Execute without permission
- Response: Process termination
- Message: "Memory permission violation"

### 9.2 Compile-Time Errors

**Raw Pointer in User Mode:**
- Error: "Raw pointers not allowed in USER mode"
- Fix: Use OSPointer[T] or switch to BAREMETAL mode

**Pointer Arithmetic in User Mode:**
- Error: "Pointer arithmetic not allowed in USER mode"
- Fix: Use OSPointer indexing or switch to BAREMETAL mode

**Address-of in User Mode:**
- Error: "Address-of operator not allowed in USER mode"
- Fix: Use OSPointer or switch to BAREMETAL mode

---

## 10. Examples

### 10.1 Basic Allocation

```pascal
{$EXECMODE USER}

program Example;
var
  buf: OSPointer[Byte];
  i: integer;
begin
  // Allocate 1KB buffer
  buf := BankAlloc(1024);
  
  // Initialize buffer
  for i := 0 to 1023 do
    buf[i] := 0;
  
  // Use buffer
  buf[0] := 65;  // 'A'
  buf[1] := 66;  // 'B'
  buf[2] := 67;  // 'C'
  
  // Free buffer
  BankFree(buf);
end.
```

### 10.2 Typed Buffer

```pascal
{$EXECMODE USER}

program TypedExample;
var
  numbers: OSPointer[Integer];
  i, sum: integer;
begin
  // Allocate array of 100 integers
  numbers := BankAlloc(100 * SizeOf(Integer));
  
  // Initialize
  for i := 0 to 99 do
    numbers[i] := i * 2;
  
  // Process
  sum := 0;
  for i := 0 to 99 do
    sum := sum + numbers[i];
  
  WriteLn('Sum: ', sum);
  
  // Free
  BankFree(numbers);
end.
```

### 10.3 Bounds Checking

```pascal
{$EXECMODE USER}

program BoundsExample;
var
  buf: OSPointer[Byte];
begin
  buf := BankAlloc(10);
  
  buf[0] := 1;   // ✅ OK
  buf[9] := 2;   // ✅ OK
  buf[10] := 3;  // ❌ Runtime error: Out of bounds
  buf[-1] := 4;  // ❌ Compile-time or runtime error
  
  BankFree(buf);
end.
```

---

## 11. Comparison: Bare Metal vs User Mode

| Feature | Bare Metal Mode | User Mode |
|---------|----------------|-----------|
| Raw Pointers | ✅ Allowed | ❌ Forbidden |
| Pointer Arithmetic | ✅ Allowed | ❌ Forbidden (internal only) |
| Address-of (`@`) | ✅ Allowed | ❌ Forbidden (internal only) |
| OSPointer | ❌ Not available | ✅ Required |
| Bounds Checking | ❌ None | ✅ Runtime |
| Permission Checking | ❌ None | ✅ Runtime |
| Capability Validation | ❌ None | ✅ Runtime |
| Use Cases | OS, drivers, bare metal | Applications, games |

---

## 12. Security Considerations

### 12.1 Threat Model

**Protected Against:**
- Accidental out-of-bounds access
- Use-after-free bugs
- Memory corruption from bugs
- Information leakage (zero-on-free)

**Not Protected Against:**
- Malicious assembly code
- Hardware manipulation
- Compiler/runtime bugs
- OS kernel vulnerabilities

### 12.2 Best Practices

1. **Always check allocation success:**
   ```pascal
   buf := BankAlloc(size);
   if buf.Length = 0 then
     HandleError;
   ```

2. **Free memory promptly:**
   ```pascal
   // Use buffer
   BankFree(buf);
   buf := nil;  // Clear reference
   ```

3. **Validate indices:**
   ```pascal
   if (index >= 0) and (index < buf.Length) then
     buf[index] := value;
   ```

4. **Don't share capabilities unnecessarily:**
   ```pascal
   // Only grant when necessary
   if NeedSharing then
     BankGrant(buf, otherPID, READ_ONLY);
   ```

---

## 13. Future Enhancements

### 13.1 Potential Features

- **Reference counting**: Automatic cleanup
- **Region splitting**: Sub-allocate from larger regions
- **Memory pools**: Fast allocation for fixed-size objects
- **Copy-on-write**: Efficient sharing
- **Memory mapping**: Map files to memory

### 13.2 Platform Evolution

- **Hardware MMU support**: If available, use hardware features
- **Multi-core**: Extend to multi-processor systems
- **Virtual memory**: Add swapping if needed
- **NUMA**: Non-uniform memory access support

---

## 14. References

- **PascalOS MemoryGuard**: `PascalOS/background/PascalOS_MemoryGuard.md`
- **Type System**: `03_TypeSystem.md` (OSPointer types)
- **Semantics**: `04_Semantics.md` (OSPointer semantics)
- **Grammar**: `02_Grammar.md` (OSPointer syntax)

---

**End of Capability-Based Memory Model Specification**
