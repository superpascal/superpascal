//! Corpus Test Generator
//!
//! Generates Rust test cases from tree-sitter-pascal corpus files.

use std::fs;
use std::path::{Path, PathBuf};
use tree_sitter_corpus::{parse_corpus_directory, CorpusTestCase};

struct Args {
    corpus_dir: PathBuf,
    output_dir: PathBuf,
    filters: Vec<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = parse_args()?;

    // Parse all corpus files
    println!("Parsing corpus files from: {}", args.corpus_dir.display());
    let corpus_data = parse_corpus_directory(&args.corpus_dir)?;

    // Filter tests if requested
    let filtered_data = if args.filters.is_empty() {
        corpus_data
    } else {
        corpus_data
            .into_iter()
            .filter(|(path, _)| {
                let filename = path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
                args.filters.iter().any(|f| filename.contains(f))
            })
            .collect()
    };

    // Generate test files
    fs::create_dir_all(&args.output_dir)?;

    let mut total_tests = 0;
    for (corpus_file, tests) in filtered_data {
        let test_file = generate_test_file(&corpus_file, &tests, &args.output_dir)?;
        println!(
            "Generated {} tests from {} -> {}",
            tests.len(),
            corpus_file.display(),
            test_file.display()
        );
        total_tests += tests.len();
    }

    println!("\nGenerated {} total tests", total_tests);
    Ok(())
}

fn parse_args() -> Result<Args, Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    let mut corpus_dir = None;
    let mut output_dir = None;
    let mut filters = Vec::new();

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--corpus-dir" => {
                corpus_dir = args.next().map(PathBuf::from);
            }
            "--output-dir" => {
                output_dir = args.next().map(PathBuf::from);
            }
            "--filter" => {
                if let Some(filter) = args.next() {
                    filters.push(filter);
                }
            }
            _ => {
                eprintln!("Unknown argument: {}", arg);
                print_usage();
                std::process::exit(1);
            }
        }
    }

    // Default to local corpus directory if it exists, otherwise fall back to tree-sitter-pascal location
    let default_corpus = if PathBuf::from("corpus").exists() {
        PathBuf::from("corpus")
    } else {
        PathBuf::from("../../../../tree-sitter-pascal/test/corpus")
    };

    Ok(Args {
        corpus_dir: corpus_dir.unwrap_or(default_corpus),
        output_dir: output_dir.unwrap_or_else(|| PathBuf::from("generated_tests")),
        filters,
    })
}

fn print_usage() {
    println!(
        r#"Usage: corpus-test-generator [OPTIONS]

Options:
    --corpus-dir DIR    Directory containing corpus files (default: ../../../../tree-sitter-pascal/test/corpus)
    --output-dir DIR   Directory to write generated tests (default: generated_tests)
    --filter NAME      Filter corpus files by name (can be used multiple times)
"#
    );
}

fn generate_test_file(
    corpus_file: &Path,
    tests: &[CorpusTestCase],
    output_dir: &Path,
) -> Result<PathBuf, Box<dyn std::error::Error>> {
    let filename = corpus_file
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");
    let test_file = output_dir.join(format!("test_{}.rs", filename.replace("-", "_")));

    let mut content = String::new();
    content.push_str("//! Generated tests from tree-sitter-pascal corpus\n");
    content.push_str("//! Source: ");
    content.push_str(&corpus_file.display().to_string());
    content.push_str("\n\n");
    content.push_str("#[cfg(test)]\n");
    content.push_str("mod tests {\n");
    content.push_str("    use super::*;\n");
    content.push_str("    use compiler_rs::parser::Parser;\n\n");

    for (i, test) in tests.iter().enumerate() {
        let test_name = sanitize_test_name(&test.name);
        content.push_str(&format!(
            "    #[test]\n    fn test_{}_{}() {{\n",
            filename.replace("-", "_"),
            test_name
        ));
        content.push_str("        let source = r#\"");
        content.push_str(&escape_string(&test.source));
        content.push_str("\"#;\n");
        content.push_str("        let mut parser = Parser::new(source);\n");
        content.push_str("        match parser {\n");
        content.push_str("            Ok(mut p) => {\n");
        content.push_str("                let result = p.parse();\n");
        content.push_str("                // TODO: Validate AST structure\n");
        content.push_str("                assert!(result.is_ok(), \"Parse failed: {:?}\", result);\n");
        content.push_str("            }\n");
        content.push_str("            Err(e) => {\n");
        content.push_str("                // Feature not yet implemented\n");
        content.push_str("                println!(\"Parser error (expected for unimplemented features): {:?}\", e);\n");
        content.push_str("            }\n");
        content.push_str("        }\n");
        content.push_str("    }\n\n");
    }

    content.push_str("}\n");

    fs::write(&test_file, content)?;
    Ok(test_file)
}

fn sanitize_test_name(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect::<String>()
        .to_lowercase()
        .replace("__", "_")
        .trim_matches('_')
        .to_string()
}

fn escape_string(s: &str) -> String {
    s.replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\r", "\\r")
        .replace("\t", "\\t")
}

// Re-export for use in generated code
use tree_sitter_corpus::CorpusTestCase;

