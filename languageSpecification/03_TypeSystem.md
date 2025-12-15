# SuperPascal Language Specification — Type System

## Complete Type System Specification

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Type System Overview

SuperPascal uses a **hybrid type system**:

- **Nominal typing** for classes and records (type identity by name)
- **Structural typing** for primitive and composite types (compatibility by structure)
- **Static typing** (all types resolved at compile time)
- **Strong typing** (explicit conversions required in most cases)

---

## 2. Primitive Types

### 2.1 Integer Types

#### `integer`
- **Size**: 16 bits (2 bytes)
- **Range**: -32768 to 32767
- **Signed**: Yes
- **Default**: Yes (for integer literals in range)
- **Operations**: All arithmetic, comparison, bitwise (via library)

#### `byte`
- **Size**: 8 bits (1 byte)
- **Range**: 0 to 255
- **Signed**: No
- **Operations**: All arithmetic, comparison

#### `word`
- **Size**: 16 bits (2 bytes)
- **Range**: 0 to 65535
- **Signed**: No
- **Operations**: All arithmetic, comparison
- **Tier**: Tier 1 (taught first)

### 2.1.2 Advanced Integer Types (Tier 2)

These types are available in Tier 2 but are not taught until after Tier 1 mastery:

#### `shortint`
- **Size**: 8 bits (1 byte)
- **Range**: -128 to 127
- **Signed**: Yes
- **Operations**: All arithmetic, comparison
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Use case**: When you need a signed 8-bit value explicitly

#### `smallint`
- **Size**: 16 bits (2 bytes)
- **Range**: -32768 to 32767
- **Signed**: Yes
- **Operations**: All arithmetic, comparison
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Note**: Semantically equivalent to `integer`, but explicit size specification

#### `longint`
- **Size**: 32 bits (4 bytes)
- **Range**: -2147483648 to 2147483647
- **Signed**: Yes
- **Operations**: All arithmetic, comparison
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Use case**: When you need 32-bit signed integers

#### `cardinal`
- **Size**: 32 bits (4 bytes)
- **Range**: 0 to 4294967295
- **Signed**: No
- **Operations**: All arithmetic, comparison
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Use case**: When you need 32-bit unsigned integers

### 2.2 Boolean Type

#### `boolean`
- **Size**: 1 byte
- **Values**: `false` (0), `true` (non-zero, typically 1)
- **Operations**: Logical (`and`, `or`, `not`), comparison (`=`, `<>`)

**Note**: Any non-zero value is considered `true` when converted to boolean.

### 2.3 Character Type

#### `char`
- **Size**: 1 byte
- **Range**: 0 to 255 (ASCII + extended)
- **Operations**: Comparison, ordinal conversion

**Character literals**: `'A'`, `#65`, escape sequences

### 2.4 String Type

#### `string` (shortstring) - Tier 1
- **Size**: Variable (1 byte length + 0-255 data bytes)
- **Maximum length**: 255 characters
- **Layout**: 
  ```
  Offset 0: Length (1 byte, 0-255)
  Offset 1+: Character data
  ```
- **Operations**: Concatenation (`+`), comparison, indexing
- **Tier**: Tier 1 (taught first)
- **String literals**: `'Hello'`, maximum 255 characters

### 2.4.2 Advanced String Types (Tier 2)

These types are available in Tier 2 but are not taught until after Tier 1 mastery:

#### `ansistring` - Tier 2
- **Size**: Variable (pointer + length + data)
- **Maximum length**: Platform-dependent (typically 2GB on 32-bit systems)
- **Layout**: Pointer to heap-allocated string data
- **Memory Management**: Manual (no garbage collection)
- **Operations**: Concatenation (`+`), comparison, indexing, substring operations
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Use case**: When you need strings longer than 255 characters or dynamic string manipulation
- **Note**: Requires explicit memory management (`New`/`Dispose` or `GetMem`/`FreeMem`)

#### `unicodestring` - Tier 2
- **Size**: Variable (pointer + length + UTF-16 data)
- **Maximum length**: Platform-dependent
- **Layout**: Pointer to heap-allocated Unicode string data (UTF-16 encoding)
- **Memory Management**: Manual (no garbage collection)
- **Operations**: Concatenation (`+`), comparison, indexing, Unicode-aware operations
- **Tier**: Tier 2 (advanced, not taught in Tier 1)
- **Use case**: When you need Unicode string support
- **Note**: Requires explicit memory management

**Educational Note:** Tier 1 students learn `string` (shortstring) which is sufficient for most programs. Tier 2 students learn when and why to use `ansistring` or `unicodestring` for advanced string manipulation.

### 2.5 Channel Types

Channels provide typed message passing between concurrent processes.

#### Synchronous Channels

**Syntax:** `channel[type1, type2, ...]`

**Characteristics:**
- Synchronous rendezvous (both sender and receiver must be ready)
- Unbuffered (direct handoff)
- Deterministic execution
- Zero-copy message transfer (stack-to-stack)

**Example:**
```pascal
type
  IntChannel = channel[integer];
  MultiChannel = channel[boolean, integer];
  
var c: IntChannel;
var value: integer;

open(c);
send(c, 42);        // Blocks until receiver ready
receive(c, value);  // Blocks until sender ready
```

**Rules:**
- Channel types are nominal (by name)
- Two channels with same type signature are different types
- Channels must be explicitly opened before use
- Synchronous channels guarantee deterministic execution order

#### Asynchronous Channels

**Syntax:** `async channel[type1, type2, ...]`

**Characteristics:**
- Asynchronous (non-blocking send if buffer available)
- Buffered (optional capacity)
- Non-deterministic execution
- Heap-allocated messages

**Example:**
```pascal
type
  AsyncIntChannel = async channel[integer][10];  // Buffered, capacity 10
  
var c: AsyncIntChannel;
var value: integer;

open(c);
send(c, 42);        // Non-blocking if buffer space available
receive(c, value);  // Blocks if buffer empty
```

**Rules:**
- Buffer capacity specified at compile time
- Send blocks only if buffer full
- Receive blocks only if buffer empty
- Higher throughput than unbuffered channels
- Execution order is non-deterministic

#### Buffered Channels

**Syntax:** `channel[type][capacity]` or `async channel[type][capacity]`

**Characteristics:**
- Buffer capacity specified at compile time
- Send blocks only if buffer full
- Receive blocks only if buffer empty
- Higher throughput than unbuffered

**Example:**
```pascal
type
  BufferedChannel = channel[integer][5];  // Synchronous, capacity 5
  AsyncBuffered = async channel[string][10];  // Asynchronous, capacity 10
  
var c: BufferedChannel;
open(c);
// Can send up to 5 messages without blocking (synchronous)
// or up to 10 messages without blocking (asynchronous)
```

**Rules:**
- Capacity must be compile-time constant
- Buffer stores messages in FIFO order
- Synchronous buffered: rendezvous if buffer empty/full
- Asynchronous buffered: non-blocking if space available

#### Channel Type Compatibility

- Channels are **nominal types** (by name)
- Two channels with same type signature are **different types**
- Channel types cannot be assigned or compared
- Channels must be explicitly opened before use
- Channel types are not compatible with other types
- No implicit conversions between channel types

#### Multi-Type Channels

Channels can accept multiple message types:

```pascal
type
  MixedChannel = channel[boolean, integer, string];
  
var c: MixedChannel;
var b: boolean;
var i: integer;
var s: string;

open(c);
send(c, true);      // Send boolean
send(c, 42);        // Send integer
send(c, 'Hello');   // Send string

receive(c, b);      // Receive boolean
receive(c, i);      // Receive integer
receive(c, s);      // Receive string
```

**Rules:**
- Type of sent value must match one of channel's allowed types
- Type of receive variable must match one of channel's allowed types
- Type checking performed at compile time
- Runtime type matching for multi-type channels

---

## 3. Ordinal Types

### 3.1 Enumeration Types

**Syntax:**
```pascal
type Color = (Red, Green, Blue);
```

**Properties:**
- **Nominal typing**: Each enum is a distinct type
- **Ordinal values**: First item = 0, increment by 1
- **Operations**: Comparison, ordinal conversion, case statements
- **Incompatibility**: Two enums with same items are different types

**Example:**
```pascal
type Color = (Red, Green, Blue);
type Mood = (Red, Green, Blue);  // Different type from Color

var c: Color;
var m: Mood;
// c := m;  // ERROR: incompatible types
```

### 3.2 Subrange Types

**Syntax:**
```pascal
type SmallInt = 1..100;
type Letter = 'A'..'Z';
```

**Properties:**
- **Base type**: Must be ordinal (integer, char, enum)
- **Bounds**: Lower ≤ Upper, both compile-time constants
- **Compatibility**: Compatible with base type if value fits
- **Range checking**: Runtime check in debug mode

**Rules:**
- Lower bound must be ≤ upper bound
- Both bounds must be of same base type
- Subrange is compatible with base type for assignment (if value fits)

---

## 4. Composite Types

### 4.1 Array Types

**Syntax:**
```pascal
type IntArray = array[0..9] of integer;
type Matrix = array[0..3, 0..3] of integer;
```

**Properties:**
- **Fixed size**: Bounds must be compile-time constants
- **Homogeneous**: All elements must be the same type
- **Structural equivalence**: Arrays with same bounds and element type are compatible
- **Multi-dimensional**: Arrays of arrays
- **Indexing**: Zero-based or custom range

**Memory Layout:**
- Elements stored contiguously
- Row-major order for multi-dimensional
- Size = (upper - lower + 1) × element_size × dimensions

**Compatibility:**
```pascal
type A1 = array[0..9] of integer;
type A2 = array[0..9] of integer;
// A1 and A2 are compatible (structural equivalence)
```

**Type Safety:**
- Arrays are **homogeneous** — all elements must be the same type
- Cannot mix types in an array: `array[0..9] of integer` can only contain integers
- Type is checked at compile time

### 4.2 List Types (SuperPascal Extension)

**Syntax:**
```pascal
var myList: list;  // Heterogeneous list (like Python)
```

**Properties:**
- **Dynamic size**: Can grow and shrink at runtime
- **Heterogeneous**: Can contain elements of different types
- **Type-safe access**: Compiler checks type when accessing elements
- **Runtime type information**: Each element stores its type

**Memory Layout:**
- Elements stored as tagged unions (type tag + value)
- Each element: `(type_tag, value)`
- Dynamic allocation (heap-based)
- Size = count × (type_tag_size + max_element_size)

**Examples:**
```pascal
var myList: list;
begin
  myList := [10, 'Hello', true, Q8.8(3.14)];  // Mixed types
  
  // Access elements (type checked at runtime)
  WriteLn(myList[0]);  // integer: 10
  WriteLn(myList[1]);  // string: 'Hello'
  WriteLn(myList[2]);  // boolean: true
  WriteLn(myList[3]);  // Q8.8: 3.14
end.
```

**Type Safety:**
- Lists are **heterogeneous** — can contain any type or combination of types
- Type is checked at runtime when accessing elements
- Type mismatch at runtime raises exception
- Use type checking functions to verify element types before access

**List Operations:**
```pascal
// Create empty list
var myList: list;
myList := [];

// Add elements (any type)
myList := myList + [10];           // Add integer
myList := myList + ['Hello'];      // Add string
myList := myList + [true];         // Add boolean
myList := myList + [Q8.8(3.14)];   // Add fixed-point

// Access elements
var value: integer;
value := myList[0];  // Runtime type check

// Type checking
if IsInteger(myList[0]) then
  value := myList[0] as integer;
```

**List vs Array:**
- **Arrays**: Homogeneous (single type), fixed size, compile-time bounds
- **Lists**: Heterogeneous (any types), dynamic size, runtime bounds

### 4.3 Result Type (SuperPascal Extension)

**Result<T, E>** is a built-in generic type for explicit error handling (Rust-style):

**Syntax:**
```pascal
type Result<T, E> = record
  case IsOk: boolean of
    true:  (Value: T);
    false: (Error: E);
  end;
```

**Properties:**
- **Tagged union** — Either `Value` (success) or `Error` (failure)
- **Zero-cost** — No overhead when no error occurs
- **Type-safe** — Compiler enforces error handling
- **Enum errors** — Use enums for error types (recommended)

**Examples:**
```pascal
type ParseError = (InvalidFormat, OutOfRange, EmptyString);

function ParseInt(s: string): Result<integer, ParseError>;
begin
  if Length(s) = 0 then
    ParseInt := Result<integer, ParseError>.Error(EmptyString)
  else
    ParseInt := Result<integer, ParseError>.Ok(StrToInt(s));
end;
```

**Usage:**
```pascal
var result: Result<integer, ParseError>;
begin
  result := ParseInt('42');
  
  case result.IsOk of
    true:  WriteLn('Value: ', result.Value);
    false: WriteLn('Error: ', result.Error);
  end;
end.
```

**Result vs Exceptions:**
- **Result<T, E>**: Use for expected errors (parsing, validation, I/O)
- **Exceptions**: Use for unexpected errors (programming errors, system failures)

### 4.4 Variant Records (Tagged Unions)

**Variant records** (also called **tagged unions**) allow a record to have different fields depending on a discriminator value. This is useful for representing data that can be one of several types.

**Syntax:**
```pascal
type
  TShape = record
    case ShapeType: integer of
      0: (X, Y, Radius: integer);        // Circle
      1: (X1, Y1, X2, Y2: integer);     // Rectangle
      2: (X1, Y1, X2, Y2, X3, Y3: integer);  // Triangle
  end;
```

**Properties:**
- **Discriminator field**: Determines which variant is active
- **Shared memory**: All variants share the same memory space
- **Type safety**: Compiler checks discriminator before accessing variant fields
- **Memory efficient**: Only one variant's fields are stored at a time

**Memory Layout:**
- Discriminator field stored first
- Variant fields share same memory offset
- Size = size of discriminator + size of largest variant

**Usage:**
```pascal
var shape: TShape;
begin
  shape.ShapeType := 0;  // Circle
  shape.X := 10;
  shape.Y := 20;
  shape.Radius := 5;
  
  case shape.ShapeType of
    0: WriteLn('Circle at (', shape.X, ', ', shape.Y, ') radius ', shape.Radius);
    1: WriteLn('Rectangle');
    2: WriteLn('Triangle');
  end;
end.
```

**Variant Records with Enums:**
```pascal
type
  ShapeKind = (Circle, Rectangle, Triangle);
  
  TShape = record
    case Kind: ShapeKind of
      Circle:    (X, Y, Radius: integer);
      Rectangle: (X1, Y1, X2, Y2: integer);
      Triangle:  (X1, Y1, X2, Y2, X3, Y3: integer);
  end;

var shape: TShape;
begin
  shape.Kind := Circle;
  shape.X := 10;
  shape.Y := 20;
  shape.Radius := 5;
  
  case shape.Kind of
    Circle:    WriteLn('Circle');
    Rectangle: WriteLn('Rectangle');
    Triangle:  WriteLn('Triangle');
  end;
end.
```

**Type Safety:**
- Compiler checks discriminator before accessing variant fields
- Accessing wrong variant's fields is undefined behavior
- Always check discriminator before accessing variant fields

**Variant Records vs Result<T, E>:**
- **Variant records**: Multiple variants of same "kind" (all shapes)
- **Result<T, E>**: Success/error dichotomy (value or error)
- Use variant records for discriminated unions
- Use Result<T, E> for error handling

### 4.5 Record Types

SuperPascal supports **two syntax styles** for records: Pascal-style `record` and C-style `struct`. Both are semantically identical.

#### Pascal-Style Syntax

**Syntax:**
```pascal
type TVec2 = record
  X, Y: integer;
end;
```

#### C-Style Struct Syntax (SuperPascal Extension)

**Syntax:**
```pascal
type TVec2 = struct {
  integer X, Y;
};
```

**Properties:**
- **Value type**: Copied by value on assignment
- **Nominal typing**: Two records compatible only if same name
- **Field layout**: Fields in declared order
- **Methods**: Can have methods (compile to free functions)
- **No inheritance**: Records cannot inherit
- **Syntax equivalence**: `record` and `struct` are interchangeable

**Memory Layout:**
- Fields stored sequentially
- No padding (except for alignment simplicity)
- Size = sum of field sizes
- Same layout regardless of syntax used

**Compatibility:**
```pascal
type R1 = record X, Y: integer; end;
type R2 = struct { integer X, Y; };
// R1 and R2 are NOT compatible (nominal typing, different names)

type R3 = struct { integer X, Y; };
type R4 = struct { integer X, Y; };
// R3 and R4 are NOT compatible (nominal typing, different type names)
```

**Anonymous Structs:**
C-style syntax supports anonymous structs:

```pascal
var point = struct {
  integer X, Y;
};
// point is an anonymous struct type
```

**Field Declaration Syntax:**
- **Pascal-style**: `X, Y: integer;` (name first, type second)
- **C-style**: `integer X, Y;` (type first, name second)

Both styles support multiple fields of the same type:
- Pascal: `X, Y, Z: integer;`
- C-style: `integer X, Y, Z;`

**Record Methods:**
```pascal
type TVec2 = record
  X, Y: integer;
  procedure Add(const B: TVec2);
end;
```

Methods compile to free functions with `var` parameter for `self`.

### 4.3 Set Types

**Syntax:**
```pascal
type CharSet = set of char;
type SmallSet = set of 0..31;
type ColorSet = set of Color;
```

**Properties:**
- **Base type**: Must be ordinal (integer, char, enum, subrange)
- **Maximum size**: 256 elements (0..255)
- **Implementation**: Bitset (1 bit per element)
- **Operations**: Union (`+`), intersection (`*`), difference (`-`), membership (`in`), equality (`=`, `<>`), subset (`<=`, `>=`)

**Set Literals:**
```pascal
var s: set of 0..31;
begin
  s := [];              // Empty set
  s := [1, 3, 5];       // Set with elements 1, 3, 5
  s := [1..10];         // Set with range 1 to 10
  s := [1, 3, 5..10];   // Mixed: elements and range
end.
```

**Set Operations:**
```pascal
var s1, s2, s3: set of 0..31;
begin
  s1 := [1, 2, 3];
  s2 := [3, 4, 5];
  
  // Union (Pascal or Python style)
  s3 := s1 + s2;        // [1, 2, 3, 4, 5] (Pascal)
  s3 := s1 | s2;        // [1, 2, 3, 4, 5] (Python)
  
  // Intersection (Pascal or Python style)
  s3 := s1 * s2;        // [3] (Pascal)
  s3 := s1 & s2;        // [3] (Python)
  
  // Difference
  s3 := s1 - s2;        // [1, 2]
  
  // Symmetric difference (Python-like)
  s3 := s1 ^ s2;        // [1, 2, 4, 5] (elements in either, but not both)
  
  if 3 in s1 then       // Membership test
    WriteLn('3 is in s1');
  
  if s1 <= s2 then      // Subset test
    WriteLn('s1 is subset of s2');
end.
```

**Set with Enums:**
```pascal
type StatusFlag = (Active, Visible, Enabled, Selected);
type StatusSet = set of StatusFlag;

var status: StatusSet;
begin
  status := [Active, Visible];
  
  if Active in status then
    WriteLn('Active');
  
  status := status + [Enabled];  // Add flag
  status := status - [Visible];   // Remove flag
end.
```

**Memory Layout:**
- Bitset: up to 32 bytes (256 bits)
- Bit N represents element N
- Set with base 0..N uses ⌈(N+1)/8⌉ bytes

**Python-Style Set Operations:**
- `|` — Union (alternative to `+`)
- `&` — Intersection (alternative to `*`)
- `^` — Symmetric difference (new)
- Both Pascal (`+`, `*`) and Python (`|`, `&`) styles are supported

**Set Methods (Python-like):**
- `Add(var s: set of T; value: T)` — Add element
- `Remove(var s: set of T; value: T)` — Remove element (raises error if not found)
- `Discard(var s: set of T; value: T)` — Remove element (no error if not found)
- `Clear(var s: set of T)` — Remove all elements
- `Copy(s: set of T): set of T` — Create copy
- `IsSubset(s1, s2: set of T): boolean` — Check if s1 ⊆ s2
- `IsSuperset(s1, s2: set of T): boolean` — Check if s1 ⊇ s2
- `IsDisjoint(s1, s2: set of T): boolean` — Check if sets have no common elements

**Compatibility:**
- Sets with same base type are compatible
- Sets with different base types are incompatible (even if ranges overlap)

### 4.4 Pointer Types

**Syntax:**
```pascal
type PInteger = ^integer;
type PVec2 = ^TVec2;
```

**Properties:**
- **Size**: 2 bytes (16-bit address)
- **Null value**: `nil`
- **Dereference**: `ptr^` accesses pointed-to value
- **Pointer arithmetic**: C-style arithmetic supported (SuperPascal extension)
- **Type safety**: Can only point to declared type

**Rules:**
- Can point to any type
- `nil` is assignable to any pointer type
- Dereferencing `nil` is undefined behavior
- Pointer arithmetic is allowed (C-style)

**Pointer Arithmetic (SuperPascal Extension):**

SuperPascal supports full C-style pointer arithmetic:

```pascal
var p, q: ^integer;
var i: integer;

p := @array[0];        // Address-of operator
p := p + 1;            // Increment by sizeof(integer)
p := p - 1;            // Decrement by sizeof(integer)
p := p + 5;            // Add integer to pointer
q := p - 3;            // Subtract integer from pointer
i := q - p;            // Pointer difference (returns integer)
```

**Pointer Arithmetic Rules:**
- **Type-aware**: Arithmetic is scaled by the size of the pointed-to type
  - `p + 1` where `p: ^integer` increments by 2 bytes (sizeof(integer))
  - `p + 1` where `p: ^byte` increments by 1 byte (sizeof(byte))
- **Pointer + Integer**: `ptr + n` advances pointer by `n * sizeof(pointee_type)` bytes
- **Pointer - Integer**: `ptr - n` decrements pointer by `n * sizeof(pointee_type)` bytes
- **Pointer - Pointer**: `ptr1 - ptr2` returns integer difference (in elements, not bytes)
  - Result is `(address1 - address2) / sizeof(pointee_type)`
- **No Pointer + Pointer**: Addition of two pointers is not allowed
- **Bounds**: No automatic bounds checking on pointer arithmetic
- **Safety**: Pointer arithmetic can access invalid memory (undefined behavior)

**Examples:**
```pascal
var
  arr: array[0..9] of integer;
  p: ^integer;
  offset: integer;

p := @arr[0];          // Point to first element
p := p + 5;            // Point to arr[5] (advances by 5 * 2 = 10 bytes)
p^ := 42;              // Set arr[5] = 42
p := p - 2;            // Point to arr[3] (decrements by 2 * 2 = 4 bytes)
offset := p - @arr[0]; // offset = 3 (element difference)
```

**Memory Layout:**
- Pointer value is 16-bit address
- Points to heap, static memory, or stack
- Arithmetic is performed in address space (scaled by type size)

---

## 5. Class Types (Object Pascal Subset)

### 5.1 Class Declaration

**Syntax:**
```pascal
type TObject = class
public
  procedure Update; virtual;
end;

type TEntity = class(TObject)
public
  ID: word;
  procedure Update; override;
end;
```

**Properties:**
- **Reference type**: Variables hold references
- **Nominal typing**: Classes compatible only if same name or inheritance
- **Single inheritance**: One parent class
- **Virtual methods**: Dynamic dispatch via vtable
- **Constructors**: Named `Create`
- **Destructors**: Named `Destroy`

### 5.2 Class Memory Layout

```
Offset 0: VTable pointer (2 bytes)
Offset 2+: Fields in declared order
```

**VTable Layout:**
```
Offset 0: Parent VTable pointer (2 bytes) or 0
Offset 2: RTTI pointer (2 bytes) or 0
Offset 4: Method slot 0 (2 bytes)
Offset 6: Method slot 1 (2 bytes)
...
```

### 5.3 Class Compatibility

**Rules:**
- Same class: Compatible
- Inheritance: Descendant assignable to ancestor
- Different classes: Not compatible (even with same structure)

**Example:**
```pascal
var obj: TObject;
var ent: TEntity;
obj := ent;      // OK: TEntity is TObject
// ent := obj;   // ERROR: TObject is not TEntity
```

### 5.4 Method Dispatch

**Non-virtual methods:**
- Resolved at compile time
- Direct call to method address

**Virtual methods:**
- Resolved at runtime via vtable
- Overridden methods replace inherited slots
- Slot index determined at compile time

---

## 6. String Type Details

### 6.1 String Type Variants

```pascal
type Name = string;        // string[255] (default)
type ShortName = string[20]; // string[20]
```

**Properties:**
- Maximum length: 255 characters
- Length stored in first byte
- Data follows length byte
- Shortstring model (Pascal standard)

### 6.2 String Operations

- **Concatenation**: `s1 + s2` (creates new string)
- **Comparison**: `=`, `<>`, `<`, `<=`, `>`, `>=` (lexicographic)
- **Indexing**: `s[i]` (1-based: index 1 = first character)
- **Length**: Implicit (stored in byte 0)

**Note**: String indexing is 1-based (Pascal convention).

---

## 7. Type Compatibility

### 7.1 Assignment Compatibility

A value of type `T1` is **assignable** to variable of type `T2` if:

1. **Same type**: `T1` and `T2` are identical
2. **Type alias**: `T1` is an alias of `T2`
3. **Subrange**: `T1` is subrange of `T2`, value fits in range
4. **Inheritance**: `T1` is descendant of `T2` (for classes)
5. **Structural**: Arrays with same bounds and element type
6. **Set compatibility**: Sets with same base type

### 7.2 Expression Compatibility

For expressions (parameters, operators):

- **Same rules as assignment**, plus:
- **Numeric promotion**: Not automatic (explicit conversion required)
- **String compatibility**: All string types compatible (up to max length)

### 7.3 Type Equivalence

**Nominal equivalence** (same name):
- Classes
- Records
- Enumerations
- Type aliases

**Structural equivalence** (same structure):
- Arrays
- Sets
- Pointers (to compatible types)

---

## 8. Type Conversions

### 8.1 Implicit Conversions

**Allowed:**
- Subrange to base type (if value fits)
- Derived class to base class
- Integer literals to compatible numeric types
- `nil` to any pointer or class reference

**Not allowed:**
- Numeric types to each other (e.g., `integer` to `word`)
- Different enum types
- Different record types (even with same structure)

### 8.2 Explicit Conversions

**Type casts:**
```pascal
var i: integer;
var w: word;
w := word(i);  // Explicit conversion
```

**Allowed casts:**
- Numeric types: `integer`, `byte`, `word` (with range checking)
- Pointer types: `^T1` to `^T2` (unsafe, use with caution)
- Ordinal types: Enum to integer, integer to enum (if value valid)

**Unsafe casts:**
- Pointer casts (no runtime checking)
- Integer to enum (if value out of range)

### 8.3 Conversion Functions

Standard library provides conversion functions:
- `IntToStr(x: integer): string` — Integer to string
- `StrToInt(s: string): integer` — String to integer
- `Ord(x): integer` — Ordinal value
- `Chr(x: integer): char` — Character from code
- `IntToFloat(x: integer): Q8.8` — Integer to fixed-point (alias for `Q8.8(x)`)
- `FloatToInt(x: Q8.8): integer` — Fixed-point to integer (truncates, alias for `integer(x)`)

### 8.4 Numeric Type Casting

**Integer to Fixed-Point:**
```pascal
var i: integer;
var f: Q8.8;
begin
  i := 10;
  f := Q8.8(i);  // Convert integer to Q8.8
  // f is now 10.0 (in Q8.8 format)
end.
```

**Fixed-Point to Integer:**
```pascal
var f: Q8.8;
var i: integer;
begin
  f := 10.5;
  i := integer(f);  // Convert Q8.8 to integer (truncates)
  // i is now 10 (fractional part lost)
end.
```

**Rounding functions:**
- `Round(x: Q8.8): integer` — Round to nearest integer
- `Ceil(x: Q8.8): integer` — Round up (ceiling)
- `Floor(x: Q8.8): integer` — Round down (floor)
- `Trunc(x: Q8.8): integer` — Truncate (same as `integer(x)` for positive values)

**Type casting rules:**
- **Integer to fixed-point** — Always safe, preserves value
- **Fixed-point to integer** — Truncates fractional part (use Round, Ceil, Floor for different behavior)
- **Integer type conversions** — May overflow if value doesn't fit (use with caution)

---

## 9. Type Inference

### 9.1 Constant Inference

Constants infer type from value:

```pascal
const
  X = 42;        // integer
  Y = $FF;       // word (if > 32767) or integer
  Z = 'A';       // char
  S = 'Hello';  // string
```

### 9.2 Variable Inference

Variables must have explicit type:

```pascal
var x: integer;  // Type required
// var x;        // ERROR: type required
```

### 9.3 Function Return Type

Functions must declare return type:

```pascal
function Add(a, b: integer): integer;  // Return type required
```

---

## 10. Type Size and Layout

### 10.1 Size Calculation

**Primitive types:**
- `integer`: 2 bytes
- `byte`: 1 byte
- `word`: 2 bytes
- `boolean`: 1 byte
- `char`: 1 byte
- `string[N]`: N+1 bytes (length + data)

**Composite types:**
- Array: `(upper - lower + 1) × element_size × dimensions`
- Record: Sum of field sizes (no padding)
- Set: `⌈(max_element + 1) / 8⌉` bytes (max 32 bytes)
- Pointer: 2 bytes
- Class: 2 bytes (vtable pointer) + sum of field sizes

### 10.2 Alignment

- **No explicit alignment requirements** for Z80
- Fields stored sequentially
- Multi-byte values use little-endian (Z80 native)

### 10.3 Memory Layout Examples

```pascal
type
  TVec2 = record
    X, Y: integer;  // 2 + 2 = 4 bytes total
  end;
  
  TArray = array[0..9] of integer;  // 10 × 2 = 20 bytes
  
  TSet = set of 0..31;  // 4 bytes (32 bits)
```

---

## 11. Special Types

### 11.1 Procedure/Function Types

**Syntax:**
```pascal
type
  TProc = procedure(x: integer);
  TFunc = function(x: integer): integer;
```

**Properties:**
- Can be assigned procedure/function addresses
- Can be called like procedures/functions
- Used for callbacks, event handlers

**Limitations:**
- No closures (cannot capture variables)
- No nested procedures as values

### 11.2 Open Array Parameters

**Syntax:**
```pascal
procedure Process(const arr: array of integer);
```

**Properties:**
- Accepts arrays of any size
- Length available via `Length(arr)`
- Passed as pointer + length

### 11.3 Game Engine Types

**Entity ID:**
```pascal
type
  TEntityID = word;  // Entity identifier
const
  ENTITY_NULL = $FFFF;  // Invalid entity
```

**Component Types:**
```pascal
type
  TPosition = record X, Y: integer; end;
  TVelocity = record VX, VY: integer; end;
  TSprite = record SpriteID: byte; TileID: word; Palette: byte; Visible: boolean; end;
  TColliderAABB = record X, Y, W, H: integer; Solid: boolean; end;
```

**Animation Types:**
```pascal
type
  TKeyframe = record TileID: byte; Duration: byte; end;
  TAnimation = record Frames: ^TKeyframe; Count: byte; Loop: boolean; end;
  TAnimationState = (asIdle, asRun, asJump, asFall);
```

**See**: [09_GameEngine.md](./09_GameEngine.md) for complete game engine type system.

### 11.4 Absolute Addressing

SuperPascal supports **absolute addressing** for variables at fixed memory locations:

**Syntax:**
```pascal
var
  VariableName: Type absolute Address;
```

**Examples:**
```pascal
var
  VideoRAM: array[0..1023] of byte absolute $C000;
  StatusReg: byte absolute $8000;
  Counter: word absolute $8001;
```

**Properties:**
- Address must be compile-time constant
- Variable exists at fixed memory location
- No initialization (memory already exists)
- Useful for hardware registers and memory-mapped I/O
- Multiple variables can share addresses (union-like)

**Memory-Mapped Hardware:**
```pascal
// ZVB Video Board registers (example addresses)
var
  ZVB_MODE: byte absolute $8000;
  ZVB_CTRL: byte absolute $8001;
```

**Note**: For I/O ports, use `PortIn`/`PortOut` intrinsics, not absolute addressing.

### 11.5 Audio Types

**Audio Channel:**
```pascal
type
  TAudioChannel = record
    Frequency: word;
    Volume: byte;
    Waveform: byte;
    Active: boolean;
    Priority: byte;
  end;
```

**Sound Effect:**
```pascal
type
  TSoundEffect = record
    Data: pointer;
    Length: word;
    Priority: byte;
    Loop: boolean;
  end;
```

**Music Track:**
```pascal
type
  TMusicTrack = record
    Patterns: ^TPattern;
    PatternCount: byte;
    Order: ^byte;
    Tempo: byte;
    Speed: byte;
  end;
```

**See**: [10_AudioSystem.md](./10_AudioSystem.md) for complete audio type system.

---

## 11.6 Generic Types (Tier 2)

**Status**: Available in Tier 2, not taught in Tier 1 curriculum.

SuperPascal supports generic types (templates) for type-safe code reuse:

**Syntax:**
```pascal
type
  TList<T> = class
    Items: array of T;
    Count: integer;
    procedure Add(const Item: T);
    function Get(Index: integer): T;
  end;
```

**Generic Procedures/Functions:**
```pascal
function Max<T>(a, b: T): T;
begin
  if a > b then
    Max := a
  else
    Max := b;
end;
```

**Generic Constraints:**
```pascal
type
  TComparable<T> = class
    where T: IComparable;
    // T must implement IComparable interface
  end;
```

**Properties:**
- **Type parameterization**: Generic types can be parameterized by types
- **Monomorphization**: Compiler generates specialized versions for each concrete type
- **Type safety**: All type checking happens at compile time
- **Zero runtime cost**: No type erasure, no runtime type information needed

**Tier Classification:**
- **Tier 1**: Not taught - students learn concrete types
- **Tier 2**: Available and taught after Tier 1 mastery

**Educational Note:** Generics are an advanced feature. Students first learn to work with concrete types, then learn generics when they need to write reusable, type-safe code.

**See**: Built-in `Result<T, E>` type (section 4.3) is an example of a generic type provided by the standard library.

---

## 12. Type Checking Rules

### 12.1 Compile-Time Checks

- Type compatibility in assignments
- Parameter type matching
- Return type matching
- Operator operand types
- Array bounds (compile-time constants)
- Case label types

### 12.2 Runtime Checks (Debug Mode)

- Subrange bounds
- Array index bounds
- Pointer nil dereference (optional)
- String length overflow

### 12.3 Type Errors

Common type errors:
- Incompatible assignment
- Wrong parameter type
- Operator on incompatible types
- Missing return value
- Type not found

---

## 13. Type System Summary

**Key Principles:**
1. **Nominal typing** for user-defined types (classes, records, enums)
2. **Structural typing** for arrays and sets
3. **Strong typing** with explicit conversions
4. **Static typing** (all types known at compile time)
5. **Type safety** (runtime checks in debug mode)

**Type Categories:**
- Primitive: `integer`, `byte`, `word`, `boolean`, `char`, `string`
- Ordinal: Enums, subranges, integers, chars
- Composite: Arrays, records, sets
- Reference: Pointers, classes
- Procedural: Procedure/function types

---

**End of Type System Specification**

