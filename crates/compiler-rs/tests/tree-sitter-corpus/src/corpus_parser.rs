//! Tree-Sitter Corpus Parser
//!
//! Parses tree-sitter corpus test files and extracts test cases.

use std::fs;
use std::path::{Path, PathBuf};

/// A single test case from a corpus file
#[derive(Debug, Clone)]
pub struct CorpusTestCase {
    /// Test name (from `=== Test Name ===`)
    pub name: String,
    /// Source code to parse
    pub source: String,
    /// Expected AST in S-expression format (optional)
    pub expected_ast: Option<String>,
    /// Source file and line number for this test
    pub source_location: SourceLocation,
}

/// Location of a test case in the corpus
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: PathBuf,
    pub line_start: usize,
    pub line_end: usize,
}

/// Parse a corpus file and extract all test cases
pub fn parse_corpus_file<P: AsRef<Path>>(path: P) -> Result<Vec<CorpusTestCase>, CorpusParseError> {
    let path = path.as_ref();
    let content = fs::read_to_string(path)
        .map_err(|e| CorpusParseError::IoError {
            path: path.to_path_buf(),
            error: e,
        })?;

    parse_corpus_content(&content, path)
}

/// Parse corpus content from a string
pub fn parse_corpus_content<P: AsRef<Path>>(
    content: &str,
    source_file: P,
) -> Result<Vec<CorpusTestCase>, CorpusParseError> {
    let source_file = source_file.as_ref();
    let mut tests = Vec::new();
    let lines: Vec<&str> = content.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        // Look for test start marker: ===
        if lines[i].trim() == "===" {
            i += 1;
            if i >= lines.len() {
                break;
            }

            // Extract test name
            let name = lines[i].trim().to_string();
            i += 1;

            // Skip next === marker
            if i < lines.len() && lines[i].trim() == "===" {
                i += 1;
            }

            let line_start = i + 1; // 1-indexed for user display

            // Collect source code until we hit ---
            let mut source_lines = Vec::new();
            while i < lines.len() && lines[i].trim() != "---" {
                source_lines.push(lines[i]);
                i += 1;
            }

            let source = source_lines.join("\n");
            let line_end = i; // 1-indexed

            // Skip --- marker
            if i < lines.len() && lines[i].trim() == "---" {
                i += 1;
            }

            // Collect expected AST (optional)
            let mut expected_ast = None;
            if i < lines.len() && !lines[i].trim().is_empty() {
                let mut ast_lines = Vec::new();
                while i < lines.len() {
                    // Stop at next test or end of file
                    if lines[i].trim() == "===" {
                        break;
                    }
                    ast_lines.push(lines[i]);
                    i += 1;
                }
                let ast = ast_lines.join("\n").trim().to_string();
                if !ast.is_empty() {
                    expected_ast = Some(ast);
                }
            }

            tests.push(CorpusTestCase {
                name,
                source,
                expected_ast,
                source_location: SourceLocation {
                    file: source_file.to_path_buf(),
                    line_start,
                    line_end,
                },
            });
        } else {
            i += 1;
        }
    }

    Ok(tests)
}

/// Parse all corpus files in a directory
pub fn parse_corpus_directory<P: AsRef<Path>>(
    dir: P,
) -> Result<Vec<(PathBuf, Vec<CorpusTestCase>)>, CorpusParseError> {
    let dir = dir.as_ref();
    let mut results = Vec::new();

    for entry in fs::read_dir(dir).map_err(|e| CorpusParseError::IoError {
        path: dir.to_path_buf(),
        error: e,
    })? {
        let entry = entry.map_err(|e| CorpusParseError::IoError {
            path: dir.to_path_buf(),
            error: e,
        })?;
        let path = entry.path();

        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("txt") {
            let tests = parse_corpus_file(&path)?;
            if !tests.is_empty() {
                results.push((path, tests));
            }
        }
    }

    Ok(results)
}

/// Errors that can occur when parsing corpus files
#[derive(Debug)]
pub enum CorpusParseError {
    IoError {
        path: PathBuf,
        error: std::io::Error,
    },
    ParseError {
        path: PathBuf,
        message: String,
    },
}

impl std::fmt::Display for CorpusParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CorpusParseError::IoError { path, error } => {
                write!(f, "IO error reading {}: {}", path.display(), error)
            }
            CorpusParseError::ParseError { path, message } => {
                write!(f, "Parse error in {}: {}", path.display(), message)
            }
        }
    }
}

impl std::error::Error for CorpusParseError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_corpus() {
        let content = r#"===
Simple Test
===

procedure Main;
begin
  writeln('Hello');
end;

---

(root
  (defProc
    (declProc (kProcedure) (identifier))
    (block (kBegin) (kEnd))))
"#;

        let tests = parse_corpus_content(content, "test.txt").unwrap();
        assert_eq!(tests.len(), 1);
        assert_eq!(tests[0].name, "Simple Test");
        assert!(tests[0].source.contains("procedure Main"));
        assert!(tests[0].expected_ast.is_some());
    }

    #[test]
    fn test_parse_multiple_tests() {
        let content = r#"===
Test 1
===

code1

---

ast1

===
Test 2
===

code2

---

ast2
"#;

        let tests = parse_corpus_content(content, "test.txt").unwrap();
        assert_eq!(tests.len(), 2);
        assert_eq!(tests[0].name, "Test 1");
        assert_eq!(tests[1].name, "Test 2");
    }

    #[test]
    fn test_parse_test_without_ast() {
        let content = r#"===
Test Without AST
===

procedure Main;
begin
end;
"#;

        let tests = parse_corpus_content(content, "test.txt").unwrap();
        assert_eq!(tests.len(), 1);
        assert_eq!(tests[0].name, "Test Without AST");
        assert!(tests[0].expected_ast.is_none());
    }
}

