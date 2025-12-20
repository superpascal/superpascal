//! Tree-Sitter Corpus Test Integration
//!
//! Tools for integrating tree-sitter-pascal's test corpus into SuperPascal's test suite.

pub mod corpus_parser;

pub use corpus_parser::{
    parse_corpus_content, parse_corpus_directory, parse_corpus_file, CorpusParseError,
    CorpusTestCase, SourceLocation,
};

