# Corpus Files Setup Guide

## Quick Start

To set up corpus files for CI and local testing:

```bash
cd crates/compiler-rs/tests/tree-sitter-corpus
./sync_corpus.sh ../../../../../tree-sitter-pascal/test/corpus
```

This will copy all corpus files into the `corpus/` directory.

## Why Corpus Files Are in the Repository

The corpus files from `tree-sitter-pascal` are **copied into our repository** (not submodules) because:

1. **CI Compatibility**: CI systems can run tests without needing to clone submodules
2. **Self-Contained**: All test data is in one place
3. **Fast Execution**: No need to fetch external dependencies
4. **Reliability**: Tests work even if tree-sitter-pascal repository is unavailable

## Corpus Files Location

Corpus files are stored in:
```
crates/compiler-rs/tests/tree-sitter-corpus/corpus/
```

This directory contains:
- `statements.txt`
- `routines.txt`
- `declarations.txt`
- `expressions.txt`
- `literals.txt`
- `modules.txt`
- `attributes.txt`
- `generics-delphi.txt`
- `generics-fpc.txt`
- `lambdas.txt`
- `inlinevar.txt`
- `preprocessor.txt`
- `pascocoa.txt`
- `README.md` (documentation about source)

## Updating Corpus Files

When `tree-sitter-pascal` adds new tests or updates existing ones:

1. **Sync the files**:
   ```bash
   cd crates/compiler-rs/tests/tree-sitter-corpus
   ./sync_corpus.sh ../../../../../tree-sitter-pascal/test/corpus
   ```

2. **Review changes**:
   ```bash
   git diff corpus/
   ```

3. **Commit the updates**:
   ```bash
   git add corpus/
   git commit -m "Update corpus files from tree-sitter-pascal"
   ```

## CI Integration

CI automatically uses the corpus files from the repository:

```yaml
# Example CI configuration
- name: Run corpus tests
  run: |
    cd crates/compiler-rs/tests/tree-sitter-corpus
    cargo run --bin corpus-validator -- --corpus-dir corpus
```

No submodules or external dependencies needed!

## License

The corpus files are from `tree-sitter-pascal` which is licensed under MIT. We maintain the same license for these files.

## Attribution

Corpus files are from:
- Repository: https://github.com/Isopod/tree-sitter-pascal
- License: MIT
- Source: `test/corpus/` directory

We acknowledge and thank the `tree-sitter-pascal` project for providing these comprehensive test cases.

