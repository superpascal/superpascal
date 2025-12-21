//! Compiler pipeline orchestration

use std::fs;
use std::path::PathBuf;

use backend_zealz80::{CodeGenerator, Z80Instruction};
use errors::Diagnostic;
use ir::{IRBuilder, Program};
use object_zealz80::{ObjectFile, Section, Symbol, SymbolType, SymbolVisibility};
use parser::Parser;
use runtime_spec::{TargetPlatform, capabilities};
use semantics::SemanticAnalyzer;
use semantics::feature_checker;

/// Compiler instance that orchestrates the compilation pipeline
pub struct Compiler {
    target: TargetPlatform,
    check_features: bool, // Whether to check feature compatibility
}

impl Compiler {
    /// Create a new compiler instance with default target (ZealZ80)
    pub fn new() -> Self {
        Self {
            target: TargetPlatform::ZealZ80,
            check_features: true,
        }
    }
    
    /// Create a new compiler instance for a specific target platform
    #[allow(dead_code)] // Public API method
    pub fn new_with_target(target: TargetPlatform) -> Self {
        Self {
            target,
            check_features: true,
        }
    }
    
    /// Create a compiler instance with feature checking disabled
    #[allow(dead_code)] // Public API method
    pub fn new_without_feature_check(target: TargetPlatform) -> Self {
        Self {
            target,
            check_features: false,
        }
    }
    
    /// Get the current target platform
    #[allow(dead_code)] // Public API method
    pub fn target(&self) -> TargetPlatform {
        self.target
    }
    
    /// Set the target platform
    #[allow(dead_code)] // Public API method
    pub fn set_target(&mut self, target: TargetPlatform) {
        self.target = target;
    }
    
    /// Enable or disable feature checking
    #[allow(dead_code)] // Public API method
    pub fn set_feature_checking(&mut self, enabled: bool) {
        self.check_features = enabled;
    }

    /// Compile a Pascal source file to an object file
    pub fn compile_file(&mut self, input_file: &str, output_file: Option<&str>) -> Result<(), String> {
        // Read source file
        let source = fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file '{}': {}", input_file, e))?;

        // Run compilation pipeline
        let (program, diagnostics) = self.compile_source(&source, Some(input_file.to_string()))?;

        // Check for errors
        let errors: Vec<&Diagnostic> = diagnostics
            .iter()
            .filter(|d| d.severity == errors::ErrorSeverity::Error)
            .collect();

        if !errors.is_empty() {
            self.print_diagnostics(&diagnostics);
            return Err(format!("Compilation failed with {} error(s)", errors.len()));
        }

        // Generate code
        let mut codegen = CodeGenerator::new();
        let instructions = codegen.generate(&program);

        // Create object file
        let unit_name = self.extract_unit_name(input_file);
        let mut obj_file = ObjectFile::new(unit_name);
        
        // Convert Z80 instructions to machine code (simplified - just emit assembly for now)
        // TODO: Implement proper assembler
        let code_bytes = self.instructions_to_bytes(&instructions)?;
        obj_file.add_code(&code_bytes);

        // Add symbols
        for function in &program.functions {
            obj_file.add_symbol(Symbol {
                name: function.name.clone(),
                symbol_type: SymbolType::Function,
                visibility: SymbolVisibility::Public,
                section: Section::Code,
                offset: 0, // TODO: Calculate actual offset
                size: 0,   // TODO: Calculate actual size
            });
        }

        // Write object file
        let output_path = output_file
            .map(|s| s.to_string())
            .unwrap_or_else(|| self.default_output_file(input_file));
        
        let mut file = fs::File::create(&output_path)
            .map_err(|e| format!("Failed to create output file '{}': {}", output_path, e))?;
        
        obj_file.write(&mut file)
            .map_err(|e| format!("Failed to write object file: {}", e))?;

        println!("Generated: {}", output_path);
        Ok(())
    }

    /// Type check a file without generating code
    pub fn check_file(&mut self, input_file: &str) -> Result<(), String> {
        let source = fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file '{}': {}", input_file, e))?;

        let (_, diagnostics) = self.compile_source(&source, Some(input_file.to_string()))?;

        // Print diagnostics
        self.print_diagnostics(&diagnostics);

        // Check for errors
        let errors: Vec<&Diagnostic> = diagnostics
            .iter()
            .filter(|d| d.severity == errors::ErrorSeverity::Error)
            .collect();

        if !errors.is_empty() {
            return Err(format!("Type checking failed with {} error(s)", errors.len()));
        }

        Ok(())
    }

    /// Emit AST for debugging
    pub fn emit_ast(&mut self, input_file: &str) -> Result<(), String> {
        let source = fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file '{}': {}", input_file, e))?;

        // Parse (parser has its own lexer)
        let mut parser = Parser::new_with_file(&source, Some(input_file.to_string()))
            .map_err(|e| format!("Parse error: {}", e))?;
        let ast = parser.parse().map_err(|e| {
            let diag = parser.error_to_diagnostic(&e);
            format!("Parse error: {}", diag)
        })?;

        // Print AST
        println!("{:#?}", ast);
        Ok(())
    }

    /// Emit IR for debugging
    pub fn emit_ir(&mut self, input_file: &str) -> Result<(), String> {
        let source = fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file '{}': {}", input_file, e))?;

        let (program, diagnostics) = self.compile_source(&source, Some(input_file.to_string()))?;

        // Print diagnostics
        self.print_diagnostics(&diagnostics);

        // Check for errors
        let errors: Vec<&Diagnostic> = diagnostics
            .iter()
            .filter(|d| d.severity == errors::ErrorSeverity::Error)
            .collect();

        if !errors.is_empty() {
            return Err(format!("Compilation failed with {} error(s)", errors.len()));
        }

        // Print IR
        println!("{:#?}", program);
        Ok(())
    }

    /// Emit assembly code
    pub fn emit_assembly(&mut self, input_file: &str) -> Result<(), String> {
        let source = fs::read_to_string(input_file)
            .map_err(|e| format!("Failed to read file '{}': {}", input_file, e))?;

        let (program, diagnostics) = self.compile_source(&source, Some(input_file.to_string()))?;

        // Print diagnostics
        self.print_diagnostics(&diagnostics);

        // Check for errors
        let errors: Vec<&Diagnostic> = diagnostics
            .iter()
            .filter(|d| d.severity == errors::ErrorSeverity::Error)
            .collect();

        if !errors.is_empty() {
            return Err(format!("Compilation failed with {} error(s)", errors.len()));
        }

        // Generate assembly
        let mut codegen = CodeGenerator::new();
        let instructions = codegen.generate(&program);

        // Print assembly
        for inst in &instructions {
            println!("{}", inst);
        }

        Ok(())
    }

    /// Core compilation pipeline
    fn compile_source(&mut self, source: &str, filename: Option<String>) -> Result<(Program, Vec<Diagnostic>), String> {
        // 1. Parsing (parser has its own lexer)
        let mut parser = Parser::new_with_file(source, filename.clone())
            .map_err(|e| format!("Parse error: {}", e))?;
        let ast = parser.parse().map_err(|e| {
            let diag = parser.error_to_diagnostic(&e);
            format!("Parse error: {}", diag)
        })?;

        // 3. Semantic Analysis
        let mut analyzer = SemanticAnalyzer::new(filename.clone());
        let mut diagnostics = analyzer.analyze(&ast);
        
        // 4. Feature Compatibility Checking
        if self.check_features {
            let capabilities = capabilities::get_capabilities(self.target);
            let mut feature_checker = feature_checker::FeatureChecker::new(capabilities, filename);
            feature_checker.check(&ast);
            diagnostics.extend_from_slice(feature_checker.diagnostics());
        }

        // 5. IR Generation (simplified - for now, create empty program)
        // TODO: Implement AST to IR conversion
        let ir_builder = IRBuilder::new();
        let program = ir_builder.into_program();

        Ok((program, diagnostics))
    }

    /// Print diagnostics to stderr
    fn print_diagnostics(&self, diagnostics: &[Diagnostic]) {
        for diagnostic in diagnostics {
            eprintln!("{}", diagnostic);
        }
    }

    /// Extract unit name from file path
    fn extract_unit_name(&self, file_path: &str) -> String {
        PathBuf::from(file_path)
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("main")
            .to_string()
    }

    /// Generate default output filename
    fn default_output_file(&self, input_file: &str) -> String {
        PathBuf::from(input_file)
            .with_extension("zof")
            .to_string_lossy()
            .to_string()
    }

    /// Convert Z80 instructions to bytes (simplified placeholder)
    /// TODO: Implement proper assembler
    fn instructions_to_bytes(&self, _instructions: &[Z80Instruction]) -> Result<Vec<u8>, String> {
        // For now, return empty - proper assembly will be implemented later
        // This is a placeholder that will be replaced with a real assembler
        Ok(vec![])
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

