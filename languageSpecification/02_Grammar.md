# SuperPascal Language Specification — Grammar

## Complete Context-Free Grammar in Extended Backus-Naur Form (EBNF)

**Version:** 1.0  
**Part of:** SuperPascal Language Specification

---

## 1. Notation

This grammar uses Extended Backus-Naur Form (EBNF) with the following conventions:

```
A ::= B          Production rule: A is defined as B
A | B            Choice: A or B
A?               Optional: zero or one occurrence of A
A*               Zero or more occurrences of A
A+               One or more occurrences of A
"literal"        Terminal symbol (keyword, operator, delimiter)
ident            Non-terminal symbol
```

**Terminal symbols** (tokens) are defined in the Lexical Structure specification.

---

## 2. Top-Level Constructs

### 2.1 Program

```
program ::= "program" ident program-params? ";" block "."
program-params ::= "(" ident-list ")"
```

**Examples:**
```pascal
program Hello;
begin
  WriteLn('Hello, SuperPascal!');
end.
```

### 2.2 Unit

```
unit ::= namespace-decl? "unit" qualified-ident ";" unit-parts "."
qualified-ident ::= ident ("." ident)*
                   | "::" ident ("." ident)*  // Global namespace (future)
unit-parts ::= interface-part implementation-part init-part?
interface-part ::= "interface" uses-clause? using-stmt* decl-section*
implementation-part ::= "implementation" uses-clause? using-stmt* decl-section*
init-part ::= "begin" statement-seq "end"
uses-clause ::= "uses" qualified-ident-list ";"
qualified-ident-list ::= qualified-ident ("," qualified-ident)*
namespace-decl ::= "namespace" qualified-ident ";"  // Future: explicit namespace
using-stmt ::= "using" qualified-ident ";"  // Future: namespace import
namespace-alias ::= "namespace" ident "=" qualified-ident ";"  // Future: namespace alias
```

**Examples:**
```pascal
// Simple unit (flat name)
unit Math;
interface
  function Sin(x: integer): integer;
implementation
  function Sin(x: integer): integer;
  begin
    // implementation
  end;
end.

// Qualified unit name (Rust-style module)
unit math.Vector2D;
interface
  type
    TVector2D = record
      X, Y: integer;
    end;
  function VectorAdd(v1, v2: TVector2D): TVector2D;
implementation
  // ...
end.

// Using qualified units
program Game;
uses
  math,              // Import math module
  math.Vector2D;     // Import Vector2D submodule
begin
  var v: math.Vector2D.TVector2D;
  // ...
end.
```

**Note:** Qualified unit names (e.g., `math.Vector2D`) are part of the planned Rust-style module system. The grammar supports both flat names (`Math`) and qualified names (`math.Vector2D`).

---

## 3. Declarations

### 3.1 Declaration Sections

```
decl-section ::= label-section
                | const-section
                | type-section
                | var-section
                | routine-decl
                | class-decl
                | directive
```

### 3.2 Labels

```
label-section ::= "label" label ("," label)* ";"
label ::= ident | integer-literal
```

**Note**: Labels are rarely used in modern SuperPascal. `goto` is supported but discouraged.

### 3.3 Constants

```
const-section ::= "const" const-decl+
const-decl ::= ident "=" const-expr ";"
const-expr ::= expr  // Must be evaluable at compile time
```

**Examples:**
```pascal
const
  MAX_SIZE = 100;
  PI = 3.14159;
  MESSAGE = 'Hello';
```

### 3.4 Types

```
type-section ::= "type" type-decl+
type-decl ::= ident "=" type-spec ";"
```

### 3.5 Variables

```
var-section ::= "var" var-decl+
var-decl ::= ident-list ":" type-spec absolute-clause? ";"
absolute-clause ::= "absolute" const-expr
ident-list ::= ident ("," ident)*
```

**Examples:**
```pascal
var
  x, y: integer;
  name: string;
  position: TVec2;
```

---

## 4. Type Specifications

### 4.1 Type Spec

```
type-spec ::= simple-type
            | subrange-type
            | enum-type
            | array-type
            | list-type
            | record-type
            | struct-type
            | set-type
            | pointer-type
            | class-type
            | string-type
            | channel-type
```

### 4.2 Simple Types

```
simple-type ::= ident  // Predefined or user-defined type name
```

**Predefined types:**
- `integer` (16-bit signed)
- `byte` (8-bit unsigned)
- `word` (16-bit unsigned)
- `boolean` (1 byte)
- `char` (8-bit)
- `string` (shortstring)

### 4.3 Subrange Types

```
subrange-type ::= const-expr ".." const-expr
```

**Examples:**
```pascal
type
  SmallInt = 1..100;
  Letter = 'A'..'Z';
```

**Rules:**
- Lower bound must be ≤ upper bound
- Both bounds must be of compatible ordinal type
- Range must fit in base type

### 4.4 Enumeration Types

```
enum-type ::= "(" enum-item ("," enum-item)* ")"
enum-item ::= ident
```

**Examples:**
```pascal
type
  Color = (Red, Green, Blue);
  Direction = (North, South, East, West);
```

**Rules:**
- Each enum item is a distinct identifier
- Enum items are ordered (ordinal values: 0, 1, 2, ...)
- Two enums with same items are different types (nominal typing)

### 4.5 Array Types

```
array-type ::= "array" "[" index-range-list "]" "of" type-spec
index-range-list ::= index-range ("," index-range)*
index-range ::= const-expr ".." const-expr
```

**Examples:**
```pascal
type
  IntArray = array[0..9] of integer;
  Matrix = array[0..3, 0..3] of integer;
  CharMap = array['A'..'Z'] of char;
```

**Rules:**
- Bounds must be compile-time constants
- Lower bound ≤ upper bound
- Multi-dimensional arrays are arrays of arrays
- **Homogeneous**: All elements must be the same type

### 4.6 List Types (SuperPascal Extension)

```
list-type ::= "list"
list-literal ::= "[" expr-list? "]"
expr-list ::= expr ("," expr)*
```

**Examples:**
```pascal
var myList: list;
begin
  myList := [];                    // Empty list
  myList := [10, 20, 30];         // List of integers
  myList := ['Hello', 'World'];    // List of strings
  myList := [10, 'Hello', true];   // Mixed types (heterogeneous)
end.
```

**Rules:**
- **Heterogeneous**: Can contain elements of different types
- Dynamic size (grows and shrinks at runtime)
- Type checked at runtime when accessing elements
- List literals can contain any valid expressions

### 4.6 Record Types

SuperPascal supports **both Pascal and C-style syntax** for record types, making it a superset of standard Pascal.

#### Pascal-Style Syntax (Standard)

```
record-type ::= "record" field-list? "end"
field-list ::= field-decl+
field-decl ::= ident-list ":" type-spec ";"
```

**Examples:**
```pascal
type
  TVec2 = record
    X, Y: integer;
  end;
  
  TPlayer = record
    Name: string;
    Position: TVec2;
    Score: integer;
  end;
```

#### C-Style Struct Syntax (SuperPascal Extension)

```
struct-type ::= "struct" ident? "{" field-list? "}" ";"
field-list ::= field-decl+
field-decl ::= type-spec ident-list ";"
```

**Examples:**
```pascal
type
  TVec2 = struct {
    integer X, Y;
  };
  
  TPlayer = struct {
    string Name;
    TVec2 Position;
    integer Score;
  };
  
  // Anonymous struct
  var point = struct {
    integer X, Y;
  };
```

**Key Differences:**
- C-style uses `struct` keyword instead of `record`
- C-style uses `{ }` braces instead of `record ... end`
- C-style field declarations: `type name;` instead of `name: type;`
- C-style requires semicolon after closing brace
- C-style supports anonymous structs (no type name)

**Semantic Equivalence:**
Both syntaxes are **semantically identical**:
- Same memory layout
- Same type compatibility rules
- Same field access syntax (`record.field`)
- Same method support (see below)

**Mixing Syntax:**
You can mix both styles in the same program:

```pascal
type
  TVec2 = record
    X, Y: integer;
  end;
  
  TPoint = struct {
    integer X, Y;
  };
  
  // Both are compatible for field access
  var v: TVec2;
  var p: TPoint;
  v.X := 10;
  p.X := 10;
```

**Note**: Type compatibility is still **nominal** - `TVec2` and `TPoint` are different types even with identical structure.

#### Record Methods

Both syntaxes support methods:

**Pascal-style:**
```pascal
type
  TVec2 = record
    X, Y: integer;
    procedure Add(const B: TVec2);
  end;
```

**C-style:**
```pascal
type
  TVec2 = struct {
    integer X, Y;
    procedure Add(const B: TVec2);
  };
```

Record methods are syntactic sugar for procedures/functions with a `var` parameter.

### 4.7 Struct Types (C-Style Extension)

```
struct-type ::= "struct" ident? "{" struct-field-list? "}" ";"
struct-field-list ::= struct-field-decl+
struct-field-decl ::= type-spec ident-list ";"
```

**Examples:**
```pascal
type
  TVec2 = struct {
    integer X, Y;
  };
  
  TPlayer = struct {
    string Name;
    TVec2 Position;
    integer Score;
  };
  
  // Anonymous struct
  var point = struct {
    integer X, Y;
  };
```

**Rules:**
- C-style field syntax: `type name;` (type first, name second)
- Requires semicolon after closing brace
- Supports anonymous structs (no type name)
- Semantically identical to `record` types
- Can be mixed with Pascal-style `record` syntax

**Struct Methods:**
```
struct-type ::= "struct" ident? "{" struct-field-list? struct-method-list? "}" ";"
struct-method-list ::= method-decl+
```

Struct methods work identically to record methods.

### 4.8 Set Types

```
set-type ::= "set" "of" ordinal-type
ordinal-type ::= simple-type | subrange-type | enum-type
```

**Examples:**
```pascal
type
  CharSet = set of char;
  SmallSet = set of 0..31;
  ColorSet = set of Color;
```

**Rules:**
- Base type must be ordinal
- Maximum set size: 256 elements (0..255)
- Sets are implemented as bitsets

### 4.9 Pointer Types

```
pointer-type ::= "^" type-spec
```

**Examples:**
```pascal
type
  PInteger = ^integer;
  PVec2 = ^TVec2;
  PPlayer = ^TPlayer;
```

**Rules:**
- Can point to any type
- `nil` is the null pointer value
- Pointer arithmetic is allowed (C-style, SuperPascal extension)
- Dereferencing `nil` is undefined behavior

### 4.10 String Type

```
string-type ::= "string" | "string" "[" integer-literal "]"
```

**Examples:**
```pascal
type
  Name = string;        // Default: string[255]
  ShortName = string[20];
```

**Rules:**
- Default string type is `string[255]` (shortstring)
- Maximum length: 255 characters
- Length stored in first byte (0-255)

### 4.11 Channel Types

```
channel-type ::= "channel" "[" type-list "]" buffer-size?
                | "async" "channel" "[" type-list "]" buffer-size?
type-list ::= type-spec ("," type-spec)*
buffer-size ::= "[" const-expr "]"  // Optional buffering capacity
```

**Examples:**
```pascal
type
  IntChannel = channel[integer];                    // Synchronous, unbuffered
  MultiChannel = channel[boolean, integer];         // Synchronous, multiple types
  BufferedChannel = channel[integer][10];          // Synchronous, buffered (capacity 10)
  AsyncChannel = async channel[integer];           // Asynchronous, unbuffered
  AsyncBuffered = async channel[string][5];        // Asynchronous, buffered
```

**Rules:**
- `channel` = synchronous rendezvous (SuperPascal-style)
- `async channel` = asynchronous buffered (May-style)
- Buffer size is optional (default: unbuffered)
- Type list specifies allowed message types
- Channels must be opened before use
- Synchronous channels: both sender and receiver must be ready (rendezvous)
- Asynchronous channels: non-blocking send if buffer space available

### 4.12 Class Types

```
class-type ::= "class" class-ancestor? class-body "end"
class-ancestor ::= "(" ident ")"
class-body ::= class-section*
class-section ::= visibility ":" class-member*
visibility ::= "public" | "private" | "protected"
class-member ::= field-decl | method-decl
```

**Examples:**
```pascal
type
  TObject = class
  public
    procedure Update; virtual;
  end;
  
  TEntity = class(TObject)
  public
    ID: word;
    procedure Update; override;
  end;
```

**Method Declarations:**
```
method-decl ::= proc-heading method-modifiers? ";"
              | func-heading method-modifiers? ":" type-spec ";"
method-modifiers ::= "virtual" | "override"
```

---

## 5. Routines: Procedures and Functions

### 5.1 Procedure Declaration

```
proc-decl ::= proc-heading ";" block ";"
proc-heading ::= "procedure" ident param-list?
```

### 5.2 Function Declaration

```
func-decl ::= func-heading ";" block ";"
func-heading ::= "function" ident param-list? ":" type-spec
```

### 5.3 Parameter Lists

```
param-list ::= "(" param-group (";" param-group)* ")"
param-group ::= param-modifier? ident-list ":" type-spec
param-modifier ::= "var" | "const"
```

**Examples:**
```pascal
procedure Swap(var a, b: integer);
function Max(a, b: integer): integer;
procedure Process(const data: array of integer);
```

**Rules:**
- `var`: pass by reference (can modify)
- `const`: pass by value, but compiler may optimize (cannot modify)
- Default: pass by value (copy)
- Parameters evaluated left-to-right

### 5.4 Function Result

Functions return values via:
- Implicit `Result` variable (SuperPascal extension)
- Explicit return statement (future)

**Example:**
```pascal
function Add(a, b: integer): integer;
begin
  Result := a + b;
end;
```

---

## 6. Statements

### 6.1 Statement

```
statement ::= simple-stmt
            | structured-stmt
            | empty
empty ::=  // Empty statement (semicolon alone)
```

### 6.2 Simple Statements

```
simple-stmt ::= assign-stmt
              | call-stmt
              | goto-stmt
              | intrinsic-stmt
              | channel-open-stmt
              | channel-send-stmt
              | channel-receive-stmt
```

### 6.3 Assignment

```
assign-stmt ::= variable ":=" expr
                | variable ":=" struct-initializer  // C-style initialization
variable ::= ident
            | variable "." ident        // Record/class/struct field
            | variable "[" expr-list "]"  // Array index
            | variable "^"                // Pointer dereference

struct-initializer ::= "{" init-list? "}"
init-list ::= init-item ("," init-item)*
init-item ::= expr | ident ":" expr
```

**Examples:**
```pascal
x := 5;
player.Position.X := 10;
array[i] := value;
ptr^ := data;

// C-style initialization
var v: TVec2 = {10, 20};
var v2: TVec2 = {X: 10, Y: 20};
var arr: array[0..2] of integer = {1, 2, 3};
```

### 6.4 Procedure Call

```
call-stmt ::= ident param-args?
            | variable "." ident param-args?  // Method call
param-args ::= "(" expr-list? ")"
expr-list ::= expr ("," expr)*
```

**Examples:**
```pascal
WriteLn('Hello');
player.Update;
vec.Add(other);
```

### 6.5 Goto

```
goto-stmt ::= "goto" label
```

**Note**: `goto` is supported but discouraged. Prefer structured control flow.

### 6.6 Structured Statements

```
structured-stmt ::= if-stmt
                  | while-stmt
                  | repeat-stmt
                  | for-stmt
                  | case-stmt
                  | try-stmt
                  | parallel-stmt
                  | forall-stmt
                  | block
```

### 6.7 If Statement

```
if-stmt ::= "if" expr "then" statement ("else" statement)?
```

**Examples:**
```pascal
if x > 0 then
  WriteLn('Positive');

if x > 0 then
  WriteLn('Positive')
else
  WriteLn('Non-positive');
```

**Dangling Else**: The `else` always binds to the nearest `if`.

### 6.8 While Statement

```
while-stmt ::= "while" expr "do" statement
```

**Example:**
```pascal
while i < 10 do
begin
  Process(i);
  i := i + 1;
end;
```

### 6.9 Repeat Statement

```
repeat-stmt ::= "repeat" statement-seq "until" expr
```

**Example:**
```pascal
repeat
  ReadInput;
  Process;
until Done;
```

**Note**: Body executes at least once (test at end).

### 6.10 For Statement

```
for-stmt ::= "for" ident ":=" expr direction expr "do" statement
direction ::= "to" | "downto"
```

**Examples:**
```pascal
for i := 1 to 10 do
  Process(i);

for i := 10 downto 1 do
  Process(i);
```

**Rules:**
- Loop variable must be ordinal type
- Loop variable is read-only in loop body
- Initial and final values evaluated once
- Direction determines step (+1 for `to`, -1 for `downto`)

### 6.11 Case Statement

```
case-stmt ::= "case" expr "of" case-branch+ ("else" statement-seq)? "end"
case-branch ::= case-labels ":" statement ";"
case-labels ::= case-label ("," case-label)*
case-label ::= const-expr | const-expr ".." const-expr
```

**Examples:**
```pascal
case x of
  1: WriteLn('One');
  2, 3: WriteLn('Two or Three');
  4..6: WriteLn('Four to Six');
else
  WriteLn('Other');
end;
```

**Rules:**
- Expression must be ordinal type
- Labels must be compile-time constants
- Labels must be unique
- Ranges allowed in labels
- `else` clause is optional

### 6.12 Try Statement

```
try-stmt ::= "try" statement-seq
             ("except" exception-handler)?
             ("finally" statement-seq)?
             "end"
exception-handler ::= statement-seq
                   | "on" ident ":" ident "do" statement
```

**Examples:**
```pascal
try
  DangerousOperation;
except
  HandleError;
end;

try
  UseResource;
finally
  CleanupResource;
end;
```

**Rules:**
- `try..except`: catches exceptions
- `try..finally`: always executes cleanup
- `try..except..finally`: both handlers allowed
- `raise` rethrows or raises new exception

### 6.13 Block

```
block ::= "begin" statement-seq "end"
statement-seq ::= statement (";" statement)*
```

**Note**: Final semicolon before `end` is optional.

### 6.14 Parallel Statement

```
parallel-stmt ::= "parallel" process-list "endparallel"
process-list ::= process-item ("|" process-item)*
process-item ::= "process" statement-seq "endprocess"
```

**Examples:**
```pascal
parallel
  process
    ComputeA;
  endprocess |
  process
    ComputeB;
  endprocess
endparallel
```

**Rules:**
- Processes execute concurrently
- All processes must complete before `endparallel`
- Processes cannot share mutable state (compile-time checked)
- Processes can share read-only data
- Process separator is `|` (pipe character)

### 6.15 Forall Statement

```
forall-stmt ::= "forall" ident ":=" expr "to" expr "do" statement
```

**Examples:**
```pascal
forall i := 1 to 100 do
  Process(i);

forall i := 0 to n-1 do
  array[i] := array[i] * 2;
```

**Rules:**
- Creates dynamic number of processes (one per iteration)
- Loop variable is read-only in body
- All processes execute concurrently
- All processes must complete before continuing
- Processes cannot share mutable state (compile-time checked)
- Initial and final expressions evaluated once

### 6.16 Channel Operations

#### 6.16.1 Open Channel

```
channel-open-stmt ::= "open" "(" variable ")"
```

**Example:**
```pascal
var c: channel[integer];
open(c);
```

**Rules:**
- Must be called before using channel
- Channel can only be opened once
- Opening an already-open channel is an error

#### 6.16.2 Send Message

```
channel-send-stmt ::= "send" "(" variable "," expr ")"
```

**Example:**
```pascal
var c: channel[integer];
send(c, 42);
```

**Rules:**
- Expression type must match channel type
- For synchronous channels: blocks until receiver ready
- For asynchronous channels: non-blocking if buffer space available
- Channel must be open

#### 6.16.3 Receive Message

```
channel-receive-stmt ::= "receive" "(" variable "," variable ")"
```

**Example:**
```pascal
var c: channel[integer];
var value: integer;
receive(c, value);
```

**Rules:**
- Variable type must match channel type
- For synchronous channels: blocks until sender ready
- For asynchronous channels: blocks if buffer empty
- Message is copied to variable
- Channel must be open

---

## 7. Expressions

### 7.1 Expression Hierarchy

Expressions are defined with operator precedence:

```
expr ::= simple-expr (rel-op simple-expr)?
simple-expr ::= term (add-op term)*
term ::= factor (mul-op factor)*
factor ::= unary-expr
unary-expr ::= primary | unary-op unary-expr
primary ::= literal
          | ident
          | "(" expr ")"
          | function-call
          | variable-access
```

### 7.2 Operator Precedence

From lowest to highest:

1. **Assignment**: `:=` (not an expression operator)
2. **Relational**: `=`, `<>`, `<`, `<=`, `>`, `>=`, `in`
3. **Additive**: `+`, `-`, `or`
4. **Multiplicative**: `*`, `/`, `div`, `mod`, `and`
5. **Unary**: `not`, `-` (unary minus), `+` (unary plus), `@` (address-of)
6. **Postfix**: `.` (field access), `[ ]` (array index), `^` (dereference), `()` (function call)

### 7.3 Relational Operators

```
rel-op ::= "=" | "<>" | "<" | "<=" | ">" | ">=" | "in"
```

**Operands:**
- `=`, `<>`: any comparable types
- `<`, `<=`, `>`, `>=`: ordinal types, strings
- `in`: membership test (Python-like)
  - `value in set`: Check if value is in set
  - `value in array`: Check if value is in array (SuperPascal extension)
  - `value in list`: Check if value is in list (SuperPascal extension)
  - `substring in string`: Check if substring is in string (SuperPascal extension)
  - `value in tuple`: Check if value is in tuple (SuperPascal extension)

### 7.4 Additive Operators

```
add-op ::= "+" | "-" | "or"
```

**Operands:**
- `+`, `-`: numeric types, strings (concatenation), sets (union/difference), arrays (concatenation - SuperPascal extension), pointers (arithmetic - SuperPascal extension)
- `|`: sets (union, alternative to `+` - SuperPascal extension)
- `or`: boolean types (logical OR, short-circuit)

**Array Concatenation (SuperPascal extension):**
- `arr1 + arr2` — Concatenate two arrays (returns new array)
- Arrays must have compatible element types
- Result array must be large enough to hold both arrays

**Pointer Arithmetic (SuperPascal extension):**
- `ptr + integer` — Add integer to pointer (type-aware scaling)
- `ptr - integer` — Subtract integer from pointer (type-aware scaling)
- `ptr1 - ptr2` — Pointer difference (returns integer, in elements)
- Pointer arithmetic is scaled by the size of the pointed-to type
- No automatic bounds checking on pointer arithmetic
- Accessing invalid memory via pointer arithmetic is undefined behavior

### 7.5 Multiplicative Operators

```
mul-op ::= "*" | "/" | "div" | "mod" | "and"
```

**Operands:**
- `*`, `/`, `div`, `mod`: numeric types, sets (intersection)
- `&`: sets (intersection, alternative to `*` - SuperPascal extension)
- `^`: sets (symmetric difference - SuperPascal extension)
- `and`: boolean types (logical AND, short-circuit)

### 7.6 Unary Operators

```
unary-op ::= "not" | "-" | "+" | "@"
```

**Operands:**
- `not`: boolean (logical NOT)
- `-`, `+`: numeric (unary minus/plus)
- `@`: any variable (address-of, returns pointer)

### 7.7 Primary Expressions

```
primary ::= literal
          | ident
          | "(" expr ")"
          | function-call
          | variable-access
          | set-constructor
```

### 7.8 Function Calls

```
function-call ::= ident "(" expr-list? ")"
                | variable "." ident "(" expr-list? ")"  // Method call
```

### 7.9 Variable Access

```
variable-access ::= ident
                  | variable-access "." ident           // Field access
                  | variable-access "[" expr-list "]"   // Array index
                  | variable-access "[" slice-expr "]"  // Array slice (SuperPascal extension)
                  | variable-access "^"                  // Pointer dereference

slice-expr ::= slice-start? ":" slice-end? (":" slice-step)?
slice-start ::= expr
slice-end ::= expr
slice-step ::= expr
```

**Examples:**
```pascal
x                    // Simple variable
player.Position.X    // Record field
array[i]             // Array element
array[1:5]           // Array slice (SuperPascal extension)
array[1:5:2]         // Array slice with step
array[:5]            // Slice from beginning
array[3:]            // Slice to end
array[:]             // Copy entire array
array[::-1]          // Reverse array
ptr^                 // Pointer dereference
ptr^.Field           // Combined
```

**Slice syntax (SuperPascal extension):**
- `arr[start:end]` — Elements from `start` to `end-1` (end exclusive, Python-style)
- `arr[start:end:step]` — Elements with step
- `arr[:end]` — From beginning to `end-1`
- `arr[start:]` — From `start` to end
- `arr[:]` — All elements (copy)
- `arr[-n:]` — Last n elements (negative index)
- `arr[::-1]` — Reverse array

**Slice semantics:**
- Returns new array with sliced elements
- End index is exclusive (like Python)
- Negative indices count from end
- Out-of-bounds indices are clamped
- Step can be positive (forward) or negative (reverse)

### 7.10 Set Constructors

```
set-constructor ::= "[" set-element-list? "]"
set-element-list ::= set-element ("," set-element)*
set-element ::= expr | expr ".." expr
```

**Examples:**
```pascal
[1, 3, 5]           // Set of specific values
[1..10]             // Set of range
[1, 3, 5..10]       // Mixed
[]                  // Empty set
```

---

## 8. SuperPascal Extensions

### 8.1 Intrinsics

```
intrinsic-stmt ::= intrinsic-name param-args?
intrinsic-name ::= "WaitVBlank"
                  | "WriteReg"
                  | "DMA_Copy"
                  | "MapPage"
                  | "SpriteSet"
                  | "SpriteShow"
                  | "FadeMusic"
                  | "Crossfade"
                  | "ReadInput"
                  | "ClearScreen"
                  // ... (see Intrinsics specification)
```

**Examples:**
```pascal
WaitVBlank;
WriteReg($1234, $56);
DMA_Copy(src, dest, size);
```

### 8.2 Tilemap DSL

```
tilemap-ctor ::= "(" tilemap-props ")"
tilemap-props ::= "Width:" integer-literal ";"
                  "Height:" integer-literal ";"
                  "Data:" "(" tilemap-rows ")"
tilemap-rows ::= tilemap-row ("," tilemap-row)*
tilemap-row ::= "ROW" "(" string-literal ")"
```

**Example:**
```pascal
const
  Level1 = (
    Width: 10;
    Height: 8;
    Data: (
      ROW('##########'),
      ROW('#........#'),
      ROW('#..####..#'),
      ROW('#........#'),
      ROW('##########')
    )
  );
```

### 8.3 Compiler Directives

```
directive ::= "{$" directive-name directive-args? "}"
directive-name ::= "INLINE"
                 | "UNROLL"
                 | "VBLANK_AWARE"
                 | "RANGE_CHECK"
                 | "OVERFLOW_CHECK"
                 | "DEBUG"
                 | "RELEASE"
                 | "IFDEF" ident
                 | "IFNDEF" ident
                 | "ENDIF"
                 // ... (see Directives specification)
directive-args ::= ident | integer-literal | string-literal
```

**Examples:**
```pascal
{$INLINE}
{$UNROLL}
{$RANGE_CHECK ON}
{$IFDEF DEBUG}
  WriteLn('Debug mode');
{$ENDIF}
```

---

## 9. Operator Precedence Table

Complete operator precedence (highest to lowest):

| Precedence | Operators | Associativity | Description |
|------------|-----------|---------------|-------------|
| 1 (highest) | `@`, `not`, `-` (unary), `+` (unary) | Right | Unary operators |
| 2 | `.`, `[ ]`, `^`, `()` | Left | Postfix operators |
| 3 | `*`, `/`, `div`, `mod`, `and` | Left | Multiplicative |
| 4 | `+`, `-`, `or` | Left | Additive |
| 5 | `=`, `<>`, `<`, `<=`, `>`, `>=`, `in` | Left | Relational |
| 6 (lowest) | `:=` | Right | Assignment (statement, not expression) |

**Note**: Assignment (`:=`) is a statement, not an expression operator.

---

## 10. Grammar Ambiguities and Resolutions

### 10.1 Dangling Else

The `else` clause always binds to the nearest `if`:

```pascal
if x > 0 then
  if y > 0 then
    WriteLn('Both positive')
  else
    WriteLn('x positive, y not');  // Binds to inner if
```

### 10.2 Range vs Field Access

- `1..10`: Range (in type, case label, set constructor)
- `record.field`: Field access (after identifier)

Resolved by context.

### 10.3 Assignment vs Type Declaration

- `x := 5`: Assignment (in statement)
- `x: integer`: Type declaration (in var/param section)

Resolved by context.

### 10.4 Unary vs Binary Minus

- `-5`: Unary minus (at start of expression, after operator)
- `a - b`: Binary minus (between expressions)

Resolved by parser state.

---

## 11. Statement Sequences and Semicolons

### 11.1 Semicolon Rules

- Semicolons separate statements in a sequence
- Final semicolon before `end` is **optional**
- Semicolon after `end` is **required** (except final `end.`)

**Examples:**
```pascal
begin
  x := 1;      // Semicolon required
  y := 2;      // Semicolon required
  z := 3       // Semicolon optional before end
end;           // Semicolon required (unless final end.)

// Final end:
begin
  x := 1;
end.           // No semicolon after final end.
```

### 11.2 Empty Statements

An empty statement (semicolon alone) is allowed:

```pascal
begin
  ;  // Empty statement
end;
```

---

## 12. Grammar Completeness

This grammar covers:

✅ All Tier 1 features (ISO 7185 Pascal core)  
✅ All Tier 2 features (Turbo Pascal extensions)  
✅ All Tier 3 features (Object Pascal subset)  
✅ Exception handling syntax  
✅ SuperPascal-specific extensions  
✅ C-style struct syntax (superset extension)  
✅ C-style initialization syntax  
✅ Operator precedence and associativity  
✅ Ambiguity resolutions  

**Superset Features:**
- C-style `struct` keyword as alternative to `record`
- C-style field declarations (`type name;`)
- C-style initialization (`{ ... }`)
- Anonymous structs

**Next**: See Semantic Rules specification for meaning and type checking rules.  
**See**: [11_LanguageExtensions.md](./11_LanguageExtensions.md) for complete extension documentation.

---

**End of Grammar Specification**

