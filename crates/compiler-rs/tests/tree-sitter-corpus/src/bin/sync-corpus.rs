//! Corpus Sync Tool
//!
//! Copies tree-sitter-pascal corpus files into our repository for CI.

use std::fs;
use std::path::{Path, PathBuf};
use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    
    // Get script directory
    let script_dir = std::env::current_exe()?
        .parent()
        .ok_or("Cannot get script directory")?
        .parent()
        .ok_or("Cannot get parent directory")?
        .parent()
        .ok_or("Cannot get parent directory")?
        .to_path_buf();
    
    let corpus_dir = script_dir.join("corpus");
    let default_source = script_dir.join("../../../../tree-sitter-pascal/test/corpus");
    
    let source_dir = if args.len() > 1 {
        PathBuf::from(&args[1])
    } else {
        default_source
    };

    println!("Syncing corpus files from: {}", source_dir.display());
    println!("Destination: {}", corpus_dir.display());

    // Check if source directory exists
    if !source_dir.exists() {
        eprintln!("Error: Source directory does not exist: {}", source_dir.display());
        eprintln!();
        eprintln!("Please provide the path to tree-sitter-pascal/test/corpus");
        eprintln!("Example: sync-corpus /path/to/tree-sitter-pascal/test/corpus");
        std::process::exit(1);
    }

    // Create corpus directory
    fs::create_dir_all(&corpus_dir)?;

    // Copy all .txt files
    println!("Copying corpus files...");
    let mut file_count = 0;
    for entry in fs::read_dir(&source_dir)? {
        let entry = entry?;
        let path = entry.path();
        
        if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("txt") {
            let filename = path.file_name().ok_or("Invalid filename")?;
            let dest = corpus_dir.join(filename);
            fs::copy(&path, &dest)?;
            println!("  Copied: {}", filename.to_string_lossy());
            file_count += 1;
        }
    }

    // Create README
    let readme_path = corpus_dir.join("README.md");
    let mut readme = fs::File::create(&readme_path)?;
    writeln!(readme, "# Tree-Sitter-Pascal Corpus Files")?;
    writeln!(readme, "")?;
    writeln!(readme, "This directory contains test corpus files copied from the `tree-sitter-pascal` project.")?;
    writeln!(readme, "")?;
    writeln!(readme, "## Source")?;
    writeln!(readme, "")?;
    writeln!(readme, "These files are from: `tree-sitter-pascal/test/corpus`")?;
    writeln!(readme, "")?;
    writeln!(readme, "Original repository: https://github.com/Isopod/tree-sitter-pascal")?;
    writeln!(readme, "")?;
    writeln!(readme, "License: MIT (same as tree-sitter-pascal)")?;
    writeln!(readme, "")?;
    writeln!(readme, "## Updating")?;
    writeln!(readme, "")?;
    writeln!(readme, "To update these files, run:")?;
    writeln!(readme, "")?;
    writeln!(readme, "```bash")?;
    writeln!(readme, "cargo run --bin sync-corpus -- [path/to/tree-sitter-pascal/test/corpus]")?;
    writeln!(readme, "```")?;
    writeln!(readme, "")?;
    writeln!(readme, "Or use the shell script:")?;
    writeln!(readme, "")?;
    writeln!(readme, "```bash")?;
    writeln!(readme, "./sync_corpus.sh [path/to/tree-sitter-pascal/test/corpus]")?;
    writeln!(readme, "```")?;
    writeln!(readme, "")?;
    writeln!(readme, "## Files")?;
    writeln!(readme, "")?;

    // List all files
    let mut files: Vec<PathBuf> = fs::read_dir(&corpus_dir)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_file() && p.extension().and_then(|s| s.to_str()) == Some("txt"))
        .collect();
    files.sort();

    for file in files {
        let filename = file.file_name().unwrap().to_string_lossy();
        writeln!(readme, "- `{}`", filename)?;
    }

    println!();
    println!("Successfully synced {} corpus files", file_count);
    println!("Files are now available at: {}", corpus_dir.display());

    Ok(())
}

