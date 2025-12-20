# Tree-Sitter-Pascal Corpus Test Integration

This directory contains tools and tests derived from `tree-sitter-pascal`'s comprehensive test corpus.

## Overview

`tree-sitter-pascal` has an extensive test corpus covering:
- **statements.txt** - Control flow statements (if, while, for, case, try, etc.)
- **routines.txt** - Procedures, functions, methods, operators
- **declarations.txt** - Variables, constants, types, classes
- **expressions.txt** - All expression types and operators
- **literals.txt** - Number, string, character literals
- **modules.txt** - Units, libraries, uses clauses
- **attributes.txt** - RTTI attributes, procedure attributes
- **generics-delphi.txt** - Delphi generic types
- **generics-fpc.txt** - FreePascal generic types
- **lambdas.txt** - Anonymous procedures/functions
- **inlinevar.txt** - Inline variable declarations
- **preprocessor.txt** - Compiler directives
- **pascocoa.txt** - PasCocoa/Objective-C interop

## Test Corpus Format

Tree-sitter corpus files use this format:

```
===
Test Name
===

source code here

---

(expected AST in S-expression format)
```

## Corpus Files Location

**Important**: Corpus files are stored in the `corpus/` directory in this repository. This ensures:
- ✅ Tests can run in CI without git submodules
- ✅ No external dependencies required
- ✅ Self-contained test suite
- ✅ Fast test execution

The corpus files are **committed to the repository** so they're always available. To update them, run the sync script and commit the changes.

## Integration Strategy

### Phase 1: Corpus Parser ✅
- Parse tree-sitter corpus files
- Extract test cases with source code and expected AST
- Convert to SuperPascal test format

### Phase 2: Test Generator ✅
- Generate Rust test cases from corpus
- Create test harness for validation
- Support filtering by feature/priority

### Phase 3: Validation ✅
- Compare SuperPascal parser output with tree-sitter expectations
- Report differences and missing features
- Track parser coverage

### Phase 4: Continuous Integration ✅
- Corpus files stored in repository (no submodules needed)
- Run corpus tests in CI
- Track parser improvements over time
- Generate coverage reports

## Setup: Syncing Corpus Files

Before using the corpus tests, you need to copy the corpus files into this repository. This ensures tests can run in CI without needing git submodules.

### Option 1: Using the Shell Script

```bash
cd crates/compiler-rs/tests/tree-sitter-corpus
./sync_corpus.sh [path/to/tree-sitter-pascal/test/corpus]
```

If `tree-sitter-pascal` is in the expected location relative to SuperPascal:

```bash
./sync_corpus.sh
```

### Option 2: Using the Rust Tool

```bash
cd crates/compiler-rs/tests/tree-sitter-corpus
cargo run --bin sync-corpus -- [path/to/tree-sitter-pascal/test/corpus]
```

### Result

After syncing, corpus files will be in `corpus/` directory:
- `corpus/statements.txt`
- `corpus/expressions.txt`
- `corpus/declarations.txt`
- ... (all corpus files)

## Usage

### Generate Tests from Corpus

```bash
cd crates/compiler-rs/tests/tree-sitter-corpus
cargo run --bin corpus-test-generator -- \
    --corpus-dir corpus \
    --output-dir generated_tests \
    --filter statements,expressions,declarations
```

If you don't specify `--corpus-dir`, it will automatically use the local `corpus/` directory if it exists.

### Run Generated Tests

```bash
cd crates/compiler-rs
cargo test --test tree_sitter_corpus
```

### Validate Parser Against Corpus

```bash
cargo run --bin corpus-validator -- \
    --corpus-dir corpus \
    --report coverage_report.json
```

If you don't specify `--corpus-dir`, it will automatically use the local `corpus/` directory if it exists.

## CI Integration

The corpus files are stored in the `corpus/` directory in this repository, so CI can run tests without needing git submodules or external dependencies.

To update corpus files (e.g., when tree-sitter-pascal adds new tests):

1. Run the sync script locally
2. Commit the updated corpus files
3. CI will automatically use the updated files

## Test Categories

Tests are organized by feature category:

- **Core** - Basic Pascal features (statements, expressions, declarations)
- **Object Pascal** - Classes, methods, properties
- **Advanced** - Generics, lambdas, preprocessor
- **Platform** - Platform-specific features (PasCocoa, etc.)

## Filtering Tests

Tests can be filtered by:
- **Feature**: Only test specific features
- **Priority**: Test high/medium/low priority features
- **Status**: Test implemented/not implemented features
- **Category**: Test core/advanced/platform features

## Expected Behavior

### Implemented Features
- Should parse successfully
- AST structure should match (where applicable)
- Error messages should be helpful

### Not Yet Implemented Features
- May fail to parse (expected)
- Should provide clear error messages
- Should not crash or panic

## Coverage Tracking

The validator tracks:
- **Parse Success Rate**: % of tests that parse successfully
- **Feature Coverage**: Which features are supported
- **Error Quality**: Quality of error messages for unsupported features
- **Performance**: Parse time for each test

## Contributing

When adding new parser features:
1. Run corpus tests to see which tests now pass
2. Update test expectations if AST structure differs
3. Add new test cases for SuperPascal-specific features
4. Update coverage report

## References

- [tree-sitter-pascal](https://github.com/Isopod/tree-sitter-pascal)
- [Tree-sitter Test Corpus Format](https://tree-sitter.github.io/tree-sitter/using-parsers#command-test)

