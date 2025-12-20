# SuperPascal Language Specification — Overview

## Complete Formal Specification for the SuperPascal Programming Language

**Version:** 1.0 (Draft)  
**Target Platforms:** Multiple retro computing platforms (ZealZ80: 8-bit, Foenix65C816: 16-bit, FoenixA2560M: 32-bit)

**Note:** Foenix support is limited to WDC65C816 processor due to its extended memory capabilities (24-bit addressing, 16MB address space). The 6502 is not supported.  
**Last Updated:** 2024

---

## Purpose

This specification defines the complete syntax, semantics, type system, and runtime behavior of SuperPascal. The core language specification is **platform-agnostic**. Platform-specific details (ABI, intrinsics, runtime) are documented in the [Platform Specifications](../platforms/README.md).

This specification serves as the authoritative reference for:

- Compiler implementers
- Language designers
- Standard library developers
- Educational material authors
- Tool developers (IDEs, debuggers, profilers)

---

## Specification Structure

This specification is organized into the following documents:

### [01_LexicalStructure.md](./01_LexicalStructure.md)
Complete lexical specification covering:
- Character set and encoding
- Identifiers and keywords
- Literals (integer, character, string, boolean)
- Operators and delimiters
- Comments and whitespace
- Compiler directive syntax
- Token classification and precedence

### [02_Grammar.md](./02_Grammar.md)
Complete context-free grammar in Extended Backus-Naur Form (EBNF):
- Program and unit structure
- Type declarations
- Variable and constant declarations
- Procedure and function declarations
- Class declarations (Object Pascal subset)
- Statement syntax
- Expression syntax with operator precedence
- Exception handling syntax
- **Execution mode directives** - `{$EXECMODE BAREMETAL}` and `{$EXECMODE USER}`
- **OSPointer syntax** - Capability pointer types and indexing
- SuperPascal-specific extensions (Tilemap DSL, intrinsics)

### [03_TypeSystem.md](./03_TypeSystem.md)
Comprehensive type system specification:
- Primitive types (integer, byte, word, boolean, char, string)
- Composite types (arrays, records, sets, pointers)
- **OSPointer[T] types** - Capability pointers for safe memory access
- Class types (Object Pascal subset)
- Type compatibility and equivalence rules
- Type conversions (implicit and explicit)
- Subrange and enumeration types
- Memory layout and size calculations
- Type inference rules (where applicable)

### [04_Semantics.md](./04_Semantics.md)
Semantic rules and meaning:
- Name resolution and scoping
- Expression evaluation order
- Assignment compatibility
- Procedure and function call semantics
- Method dispatch (virtual vs. non-virtual)
- Constant folding and evaluation
- Control flow semantics
- Memory model and lifetime rules
- Error conditions and undefined behavior

### [05_ABI_Concepts.md](./05_ABI_Concepts.md)
General ABI concepts and principles (platform-agnostic):
- What is an ABI and its components
- Parameter passing strategies (value, var, const, hidden)
- Return value strategies (scalar vs. large)
- Data layout principles (records, classes, arrays)
- Name mangling concepts
- Exception handling model
- ABI stability and versioning
- Compiler-runtime-linker responsibilities

### Platform-Specific ABI Implementations
Platform-specific ABI implementations (CPU-specific calling conventions):
- **[ZealZ80 ABI](../platforms/ZealZ80/ABI.md)** - Z80 calling conventions, register usage, stack frames
- **[Platform Specifications](../platforms/README.md)** - All platform-specific ABI implementations

**Note:** The language specification is platform-agnostic. ABI concepts are general, but ABI implementations, intrinsics, and runtime are defined per-platform.

### [06_IntrinsicsAndDirectives.md](./06_IntrinsicsAndDirectives.md)
SuperPascal language extensions:
- **Platform-agnostic intrinsics** - Basic graphics, input, audio concepts
- **Compiler directives** - Syntax and semantics (platform-agnostic)
- **Tilemap DSL** - Domain-specific language syntax
- **Platform-specific intrinsics** - See [Platform Specifications](../platforms/README.md) for hardware-specific intrinsics

### [07_Exceptions.md](./07_Exceptions.md)
Exception handling model:
- Exception syntax (try/except/finally/raise)
- Exception frame structure
- Stack unwinding semantics
- Exception propagation rules
- Integration with runtime
- Error reporting and debugging

### [08_LanguageReference.md](./08_LanguageReference.md)
Quick reference guide:
- Keyword reference
- Operator precedence table
- Type compatibility matrix
- Common patterns and idioms

### [13_StandardLibrary.md](./13_StandardLibrary.md)
Standard library specification:
- String functions and manipulation
- Type conversion functions
- Math functions (rounding, basic math, trigonometry)
- Math constants (Pi, E, Tau)
- String interpolation
- Ordinal functions
- Platform considerations
- Migration notes from standard Pascal

### [14_RustStyleModules.md](./14_RustStyleModules.md)
Rust-style module system specification (planned):
- Directory-based modules (`math/mod.pas` → `unit math;`)
- Qualified unit names (`math.Vector2D`)
- Nested modules (`graphics.sprites.Sprite`)
- Module resolution rules
- Migration from flat units
- Backward compatibility

### [11_LanguageExtensions.md](./11_LanguageExtensions.md)
Language extensions beyond standard Pascal:
- C-style struct syntax (`struct` keyword)
- C-style initialization (`{ ... }` syntax)
- Anonymous structs
- Mixed syntax support
- Future planned extensions

### [12_MemoryAndIO.md](./12_MemoryAndIO.md)
Direct memory and I/O port access:
- Memory access functions (`Peek`, `PeekW`, `Poke`, `PokeW`)
- I/O port access (`PortIn`, `PortOut`, `PortInW`, `PortOutW`)
- Absolute addressing (`absolute` directive)
- Hardware register access patterns
- Integration with Zeal hardware (RAM, PIO)

### [15_MemoryManagement.md](./15_MemoryManagement.md)
Complete memory management strategy:
- **No garbage collection** - Manual memory management philosophy
- **Value types (records)** - Stack vs heap allocation
- **Reference types (classes)** - Always heap-allocated
- **Memory management facilities** - `New`/`Dispose`, `GetMem`/`FreeMem`, `Create`/`Destroy`
- **Ownership patterns** - Single ownership, transfer, borrowing
- **RAII-like patterns** - Using `try/finally` for guaranteed cleanup
- **Memory safety rules** - Null safety, double-free prevention, dangling pointer prevention
- **Memory leak prevention** - Best practices and patterns
- **Complete strategy** - All facilities needed for the hybrid model (value + reference types)

### [17_CapabilityBasedMemory.md](./17_CapabilityBasedMemory.md)
Capability-based memory model for platforms without MMU:
- **OSPointer[T] type** - Opaque capability pointers for safe memory access
- **Two-tier pointer model** - Bare metal (raw pointers) vs User mode (capability pointers)
- **Execution mode system** - `{$EXECMODE BAREMETAL}` vs `{$EXECMODE USER}`
- **Runtime safety** - Bounds checking, permission checking, capability validation
- **Platform-specific implementations** - 65C816 (PascalOS), Z80, and others
- **Memory management API** - BankAlloc, BankFree, BankGrant, BankRevoke

### [09_GameEngine_Concepts.md](./09_GameEngine_Concepts.md)
General Game Engine concepts (platform-agnostic):
- Entity-Component-System (ECS) architecture principles
- Structure-of-Arrays (SoA) storage concepts
- Core game engine types (abstract)
- Entity management concepts
- Component access strategies
- System schedule concepts
- Change detection principles
- Platform abstraction strategy and naming conventions

### [09_GameEngine.md](./09_GameEngine.md)
Game Engine integration overview:
- Platform abstraction patterns
- Naming conventions (`<Concept>Hardware<Platform>`)
- Conditional compilation strategies
- Cross-platform game development

### Platform-Specific Game Engine Implementations
Hardware-specific game engine implementations:
- **[ZealZ80 Game Engine](../platforms/ZealZ80/gameEngine/README.md)** - ZVB hardware integration
- **[Platform Specifications](../platforms/README.md)** - All platform-specific game engine implementations

### [10_AudioSystem.md](./10_AudioSystem.md)
Audio System integration:
- Audio hardware model (ZVB)
- Sound effects system with priorities
- Pattern-based music engine
- Audio streaming for long-form content
- Channel mixing and ducking
- Scripting integration
- Standard library units for audio

---

## Language Philosophy

SuperPascal is designed with the following principles:

1. **Educational Clarity**: Simple, predictable semantics suitable for teaching
2. **Modern Pascal**: Combines classic Pascal clarity with modern features
3. **Superset Design**: Extends Pascal with C-style syntax and modern features
4. **Hybrid Model**: Records for data, classes for behavior orchestration
5. **Z80-Optimized**: Efficient code generation for 8-bit hardware
6. **Game Development**: Built-in support for ECS, graphics, and audio
7. **Safety**: Bounds checking, type safety, clear error messages

---

## Language Tiers

SuperPascal is organized into three conceptual tiers. **All Tier 2 and Tier 3 features exist in the language**, but are not taught until students master Tier 1 fundamentals.

### Tier 1: ISO 7185 Pascal Core (Foundation)
**Taught in:** Beginner curriculum  
**Available in:** All compilers (micro-compiler and full compiler)

Foundation language features:
- Primitive types: `integer`, `byte`, `word`, `boolean`, `char`, `string` (shortstring)
- Arrays, records
- Control flow (if, while, for, case, repeat)
- Procedures and functions
- Basic I/O (`Write`, `WriteLn`, `Read`, `ReadLn`)
- Basic pointers

**Educational Note:** Students learn these fundamentals before progressing to advanced features.

### Tier 2: Turbo Pascal Extensions + Advanced Features (Practical Pascal)
**Taught in:** Advanced curriculum (after Tier 1 mastery)  
**Available in:** Full compiler only

Practical programming features:
- Units (modular compilation)
- Short strings (unified `string` type in Tier 1)
- Sets, packed records
- Compiler directives (`{$INLINE}`, `{$UNROLL}`, etc.)
- File I/O extensions
- Tilemap DSL

**Advanced features (available but not taught until Tier 2):**
- Advanced integer types: `shortint`, `smallint`, `longint`, `cardinal`
- Advanced string types: `ansistring`, `unicodestring` (manual memory management)
- Generics/Templates
- Operator overloading
- Type helpers
- Attributes/Annotations

**Educational Note:** All features exist in the compiler, but are introduced progressively after Tier 1 mastery.

### Tier 3: Object Pascal Subset (Behavior Layer)
**Taught in:** Advanced curriculum (part of Tier 2)  
**Available in:** Full compiler only

Object-oriented features:
- Classes with single inheritance
- Virtual and override methods
- Constructors and destructors
- Reference semantics for classes

**Note:** Tier 3 is conceptually part of Tier 2 - it's the OOP layer within the advanced features.

---

## Hybrid OOP + Struct Model

SuperPascal uses a unique hybrid model:

- **Records/Structs** = Data containers (value types, no inheritance)
  - Used for ECS components
  - Fast SoA (Structure of Arrays) iteration
  - Methods compile to free functions
  - Supports both Pascal `record` and C-style `struct` syntax

- **Classes** = Behavior orchestration (reference types, inheritance)
  - Used for game systems, scenes, UI controllers
  - Lightweight vtables
  - Reference semantics

This design enables efficient ECS-based game development while maintaining OOP for system organization.

## Superset Features

SuperPascal extends standard Pascal (ISO 7185) with:

- **C-style struct syntax**: `struct { ... }` as alternative to `record ... end`
- **C-style initialization**: `{ ... }` initializers
- **Anonymous structs**: Structs without type names
- **Mixed syntax**: Can use both Pascal and C-style in same program

All extensions are **syntactic sugar** - they generate identical code to Pascal equivalents, ensuring full compatibility.

---

## Target Platform: Zeal 8-bit Computer

### Hardware Specifications
- **CPU**: Zilog Z80 @ 10 MHz
- **Memory**: 512 KB SRAM with 16 KB paging
- **Graphics**: FPGA-accelerated video subsystem (ZVB)
- **Audio**: FPGA-accelerated audio subsystem
- **Storage**: SD card interface

### Platform Advantages
- FPGA offloads video/audio from CPU
- Large memory enables complex programs
- Paged memory supports efficient compilation

---

## Specification Status

This specification is **complete and authoritative** for compiler implementation. All compiler phases must conform to these rules.

### Version History
- **v1.0 (Draft)**: Complete specification covering all language features
- **v0.8**: Compiler architecture plan
- **v0.7**: Exceptions and advanced features
- **v0.4**: ABI and code generation
- **v0.3**: Semantic rules
- **v0.2**: Grammar definition
- **v0.1**: High-level language specification

---

## Using This Specification

### For Compiler Writers
1. Start with [01_LexicalStructure.md](./01_LexicalStructure.md) for tokenization
2. Implement [02_Grammar.md](./02_Grammar.md) for parsing
3. Apply [03_TypeSystem.md](./03_TypeSystem.md) and [04_Semantics.md](./04_Semantics.md) for type checking
4. Use platform-specific [ABI specifications](../platforms/README.md) for code generation
5. Reference [06_IntrinsicsAndDirectives.md](./06_IntrinsicsAndDirectives.md) for platform-agnostic intrinsics concepts
6. See [Platform Specifications](../platforms/README.md) for platform-specific intrinsics

### For Language Users
- See [08_LanguageReference.md](./08_LanguageReference.md) for quick reference
- Refer to specific sections as needed
- Check examples in documentation

### For Educators
- Tier 1 features are suitable for beginners
- Tier 2 introduces practical programming
- Tier 3 covers object-oriented concepts
- All tiers include educational considerations

---

## Conformance

A SuperPascal compiler is **conforming** if it:

1. Accepts all programs that conform to this specification
2. Rejects all programs that violate this specification
3. Generates code that follows the ABI defined in [05_ABI.md](./05_ABI.md)
4. Produces behavior matching the semantics in [04_Semantics.md](./04_Semantics.md)

---

## Notation Conventions

Throughout this specification:

- **Bold** = Keywords or language constructs
- `code` = Code examples or identifiers
- *Italic* = Important concepts or terms
- `::=` = Grammar production (EBNF)
- `|` = Alternative (choice)
- `?` = Optional (zero or one)
- `*` = Zero or more
- `+` = One or more

---

## Feedback and Contributions

This specification is maintained as part of the SuperPascal project. For questions, corrections, or suggestions, please refer to the project's contribution guidelines.

---

**End of Overview**

