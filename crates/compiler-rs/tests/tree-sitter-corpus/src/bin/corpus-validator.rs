//! Corpus Validator
//!
//! Validates SuperPascal parser against tree-sitter-pascal corpus tests.

use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use tree_sitter_corpus::{parse_corpus_directory, CorpusTestCase};

#[derive(Debug, Clone)]
struct ValidationResult {
    test_name: String,
    source_file: PathBuf,
    parse_success: bool,
    error_message: Option<String>,
    parse_time_ms: u64,
}

#[derive(Debug)]
struct CoverageReport {
    total_tests: usize,
    successful_parses: usize,
    failed_parses: usize,
    parse_success_rate: f64,
    results_by_file: HashMap<PathBuf, Vec<ValidationResult>>,
    average_parse_time_ms: f64,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = parse_args()?;

    println!("Validating parser against corpus...");
    println!("Corpus directory: {}", args.corpus_dir.display());

    // Parse corpus files
    let corpus_data = parse_corpus_directory(&args.corpus_dir)?;

    // Validate each test
    let mut all_results = Vec::new();
    for (corpus_file, tests) in corpus_data {
        println!("\nValidating {} ({} tests)", corpus_file.display(), tests.len());
        for test in tests {
            let result = validate_test(&test)?;
            all_results.push(result);
        }
    }

    // Generate report
    let report = generate_report(all_results);
    print_report(&report);

    // Save report if requested
    if let Some(report_path) = args.report {
        save_report(&report, &report_path)?;
        println!("\nReport saved to: {}", report_path.display());
    }

    Ok(())
}

struct Args {
    corpus_dir: PathBuf,
    report: Option<PathBuf>,
}

fn parse_args() -> Result<Args, Box<dyn std::error::Error>> {
    let mut args = std::env::args().skip(1);
    let mut corpus_dir = None;
    let mut report = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--corpus-dir" => {
                corpus_dir = args.next().map(PathBuf::from);
            }
            "--report" => {
                report = args.next().map(PathBuf::from);
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
        report,
    })
}

fn print_usage() {
    println!(
        r#"Usage: corpus-validator [OPTIONS]

Options:
    --corpus-dir DIR    Directory containing corpus files (default: ../../../../tree-sitter-pascal/test/corpus)
    --report PATH       Path to save JSON report (optional)
"#
    );
}

fn validate_test(test: &CorpusTestCase) -> Result<ValidationResult, Box<dyn std::error::Error>> {
    use std::time::Instant;
    use compiler_rs::parser::Parser;

    let start = Instant::now();
    let parse_result = Parser::new(&test.source);
    let parse_time = start.elapsed().as_millis() as u64;

    match parse_result {
        Ok(mut parser) => {
            match parser.parse() {
                Ok(_ast) => Ok(ValidationResult {
                    test_name: test.name.clone(),
                    source_file: test.source_location.file.clone(),
                    parse_success: true,
                    error_message: None,
                    parse_time_ms: parse_time,
                }),
                Err(e) => Ok(ValidationResult {
                    test_name: test.name.clone(),
                    source_file: test.source_location.file.clone(),
                    parse_success: false,
                    error_message: Some(format!("{:?}", e)),
                    parse_time_ms: parse_time,
                }),
            }
        }
        Err(e) => Ok(ValidationResult {
            test_name: test.name.clone(),
            source_file: test.source_location.file.clone(),
            parse_success: false,
            error_message: Some(format!("{:?}", e)),
            parse_time_ms: parse_time,
        }),
    }
}

fn generate_report(results: Vec<ValidationResult>) -> CoverageReport {
    let total_tests = results.len();
    let successful_parses = results.iter().filter(|r| r.parse_success).count();
    let failed_parses = total_tests - successful_parses;
    let parse_success_rate = if total_tests > 0 {
        (successful_parses as f64 / total_tests as f64) * 100.0
    } else {
        0.0
    };

    let total_parse_time: u64 = results.iter().map(|r| r.parse_time_ms).sum();
    let average_parse_time_ms = if total_tests > 0 {
        total_parse_time as f64 / total_tests as f64
    } else {
        0.0
    };

    let mut results_by_file: HashMap<PathBuf, Vec<ValidationResult>> = HashMap::new();
    for result in results {
        results_by_file
            .entry(result.source_file.clone())
            .or_insert_with(Vec::new)
            .push(result);
    }

    CoverageReport {
        total_tests,
        successful_parses,
        failed_parses,
        parse_success_rate,
        results_by_file,
        average_parse_time_ms,
    }
}

fn print_report(report: &CoverageReport) {
    println!("\n=== Coverage Report ===");
    println!("Total Tests: {}", report.total_tests);
    println!("Successful Parses: {}", report.successful_parses);
    println!("Failed Parses: {}", report.failed_parses);
    println!("Parse Success Rate: {:.2}%", report.parse_success_rate);
    println!("Average Parse Time: {:.2}ms", report.average_parse_time_ms);

    println!("\n=== Results by File ===");
    for (file, results) in &report.results_by_file {
        let success_count = results.iter().filter(|r| r.parse_success).count();
        let total = results.len();
        let rate = if total > 0 {
            (success_count as f64 / total as f64) * 100.0
        } else {
            0.0
        };
        println!(
            "{}: {}/{} ({:.1}%)",
            file.file_name().unwrap().to_string_lossy(),
            success_count,
            total,
            rate
        );
    }
}

fn save_report(report: &CoverageReport, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
    use std::collections::BTreeMap;

    // Convert to JSON-serializable format
    let mut json_data = serde_json::Map::new();
    json_data.insert(
        "total_tests".to_string(),
        serde_json::Value::Number(report.total_tests.into()),
    );
    json_data.insert(
        "successful_parses".to_string(),
        serde_json::Value::Number(report.successful_parses.into()),
    );
    json_data.insert(
        "failed_parses".to_string(),
        serde_json::Value::Number(report.failed_parses.into()),
    );
    json_data.insert(
        "parse_success_rate".to_string(),
        serde_json::Value::Number(
            serde_json::Number::from_f64(report.parse_success_rate)
                .unwrap_or(serde_json::Number::from(0)),
        ),
    );
    json_data.insert(
        "average_parse_time_ms".to_string(),
        serde_json::Value::Number(
            serde_json::Number::from_f64(report.average_parse_time_ms)
                .unwrap_or(serde_json::Number::from(0)),
        ),
    );

    let mut file_results = serde_json::Map::new();
    for (file, results) in &report.results_by_file {
        let success_count = results.iter().filter(|r| r.parse_success).count();
        let mut file_data = serde_json::Map::new();
        file_data.insert(
            "total".to_string(),
            serde_json::Value::Number(results.len().into()),
        );
        file_data.insert(
            "successful".to_string(),
            serde_json::Value::Number(success_count.into()),
        );
        file_data.insert(
            "failed".to_string(),
            serde_json::Value::Number((results.len() - success_count).into()),
        );
        file_results.insert(
            file.to_string_lossy().to_string(),
            serde_json::Value::Object(file_data),
        );
    }
    json_data.insert("file_results".to_string(), serde_json::Value::Object(file_results));

    let json = serde_json::to_string_pretty(&json_data)?;
    fs::write(path, json)?;
    Ok(())
}

