//! Feature Compatibility Checker
//!
//! This module validates that the parsed AST only uses language features
//! supported by the target backend. It generates warnings or errors for
//! unsupported features.

use ast;
use ast::Node;
use runtime_spec::capabilities::{BackendCapabilities, LanguageFeature};
use errors::{Diagnostic, ErrorSeverity};
use tokens::Span;

/// Feature checker that validates AST against backend capabilities
pub struct FeatureChecker {
    capabilities: BackendCapabilities,
    diagnostics: Vec<Diagnostic>,
    filename: Option<String>,
}

impl FeatureChecker {
    /// Create a new feature checker
    pub fn new(capabilities: BackendCapabilities, filename: Option<String>) -> Self {
        Self {
            capabilities,
            diagnostics: vec![],
            filename,
        }
    }
    
    /// Check an AST node for unsupported features
    pub fn check(&mut self, node: &Node) {
        self.check_node(node);
    }
    
    /// Get all diagnostics
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }
    
    /// Check a single node and its children
    fn check_node(&mut self, node: &Node) {
        // Check this node for unsupported features
        if let Some(feature) = self.node_to_feature(node) {
            if !self.capabilities.supports(feature) {
                self.add_error(
                    format!(
                        "Feature '{}' is not supported on {} backend",
                        feature_name(feature),
                        self.capabilities.name
                    ),
                    node.span(),
                );
            }
        }
        
        // Recursively check children
        self.check_children(node);
    }
    
    /// Map AST node to language feature
    fn node_to_feature(&self, node: &Node) -> Option<LanguageFeature> {
        match node {
            // Dynamic arrays
            Node::ArrayType(_arr) => {
                // Static array - no feature check needed (supported on all platforms)
                None
            }
            Node::DynamicArrayType(_dyn_arr) => {
                // Dynamic arrays require heap management
                Some(LanguageFeature::DynamicArrays)
            }
            
            // Sets
            Node::SetType(_) => Some(LanguageFeature::Sets),
            Node::SetLiteral(_) => Some(LanguageFeature::Sets),
            
            // Strings
            Node::StringType(_) => Some(LanguageFeature::Strings),
            
            // Variant records
            Node::RecordType(record) => {
                if record.variant.is_some() {
                    Some(LanguageFeature::VariantRecords)
                } else {
                    None
                }
            }
            
            // Enumerated types
            Node::EnumType(_) => Some(LanguageFeature::EnumeratedTypes),
            
            // Pointers
            Node::PointerType(_) => Some(LanguageFeature::Pointers),
            Node::DerefExpr(_) => Some(LanguageFeature::Pointers),
            
            // Procedural types
            Node::ProceduralType(proc) => {
                if proc.is_method_pointer {
                    Some(LanguageFeature::MethodPointers)
                } else {
                    Some(LanguageFeature::ProceduralTypes)
                }
            }
            
            // File types
            Node::FileType(_) => Some(LanguageFeature::FileTypes),
            
            // Classes
            Node::ClassType(_) => Some(LanguageFeature::Classes),
            
            // Interfaces
            Node::InterfaceType(_) => Some(LanguageFeature::Interfaces),
            
            // Properties
            Node::PropertyDecl(_) => Some(LanguageFeature::Properties),
            
            // Operator overloading
            Node::OperatorDecl(_) => Some(LanguageFeature::OperatorOverloading),
            
            // Exception handling
            Node::TryStmt(_) => Some(LanguageFeature::ExceptionHandling),
            Node::RaiseStmt(_) => Some(LanguageFeature::ExceptionHandling),
            
            // With statement
            Node::WithStmt(_) => Some(LanguageFeature::WithStatement),
            
            // Goto/Labels
            Node::GotoStmt(_) => Some(LanguageFeature::GotoLabels),
            Node::LabelDecl(_) => Some(LanguageFeature::GotoLabels),
            Node::LabeledStmt(_) => Some(LanguageFeature::GotoLabels),
            
            // Inline assembly
            Node::AsmStmt(_) => Some(LanguageFeature::InlineAssembly),
            
            // For..in loops
            Node::ForInStmt(_) => Some(LanguageFeature::ForInLoops),
            
            // Nested routines (check in declarations)
            // This would be detected during declaration analysis
            
            _ => None,
        }
    }
    
    /// Recursively check children nodes
    fn check_children(&mut self, node: &Node) {
        match node {
            Node::Program(p) => self.check_node(&p.block),
            Node::Unit(u) => {
                if let Some(interface) = &u.interface {
                    // Check interface section children
                    if let Some(uses) = &interface.uses {
                        self.check_node(&Node::UsesClause(uses.clone()));
                    }
                    for decl in &interface.const_decls { self.check_node(decl); }
                    for decl in &interface.type_decls { self.check_node(decl); }
                    for decl in &interface.var_decls { self.check_node(decl); }
                    for decl in &interface.proc_decls { self.check_node(decl); }
                    for decl in &interface.func_decls { self.check_node(decl); }
                    for decl in &interface.operator_decls { self.check_node(decl); }
                    for decl in &interface.property_decls { self.check_node(decl); }
                }
                if let Some(implementation) = &u.implementation {
                    // Check implementation section children
                    if let Some(uses) = &implementation.uses {
                        self.check_node(&Node::UsesClause(uses.clone()));
                    }
                    for decl in &implementation.const_decls { self.check_node(decl); }
                    for decl in &implementation.type_decls { self.check_node(decl); }
                    for decl in &implementation.var_decls { self.check_node(decl); }
                    for decl in &implementation.proc_decls { self.check_node(decl); }
                    for decl in &implementation.func_decls { self.check_node(decl); }
                    for decl in &implementation.operator_decls { self.check_node(decl); }
                    for decl in &implementation.property_decls { self.check_node(decl); }
                }
                if let Some(init) = &u.initialization { self.check_node(init); }
                if let Some(finalz) = &u.finalization { self.check_node(finalz); }
            }
            Node::Block(b) => {
                for decl in &b.label_decls { self.check_node(decl); }
                for decl in &b.const_decls { self.check_node(decl); }
                for decl in &b.type_decls { self.check_node(decl); }
                for decl in &b.var_decls { self.check_node(decl); }
                // Check THREADVAR declarations
                if !b.threadvar_decls.is_empty() {
                    if !self.capabilities.supports(LanguageFeature::ThreadVar) {
                        for decl in &b.threadvar_decls {
                            self.add_error(
                                format!("THREADVAR is not supported on {} backend", self.capabilities.name),
                                decl.span(),
                            );
                        }
                    } else {
                        for decl in &b.threadvar_decls { self.check_node(decl); }
                    }
                }
                for decl in &b.proc_decls { self.check_node(decl); }
                for decl in &b.func_decls { self.check_node(decl); }
                for decl in &b.operator_decls { self.check_node(decl); }
                for stmt in &b.statements { self.check_node(stmt); }
            }
            Node::VarDecl(v) => {
                if v.absolute_address.is_some() {
                    if !self.capabilities.supports(LanguageFeature::Absolute) {
                        self.add_error(
                            format!("ABSOLUTE addressing is not supported on {} backend", self.capabilities.name),
                            v.span,
                        );
                    }
                }
                self.check_node(&v.type_expr);
            }
            Node::ConstDecl(c) => {
                if c.is_resourcestring {
                    if !self.capabilities.supports(LanguageFeature::Resourcestring) {
                        self.add_error(
                            format!("RESOURCESTRING is not supported on {} backend", self.capabilities.name),
                            c.span,
                        );
                    }
                }
                self.check_node(&c.value);
            }
            Node::TypeDecl(t) => self.check_node(&t.type_expr),
            Node::ProcDecl(p) => {
                if p.is_forward {
                    if !self.capabilities.supports(LanguageFeature::ForwardExternal) {
                        self.add_error(
                            format!("FORWARD declarations are not supported on {} backend", self.capabilities.name),
                            p.span,
                        );
                    }
                }
                if p.is_external {
                    if !self.capabilities.supports(LanguageFeature::ForwardExternal) {
                        self.add_error(
                            format!("EXTERNAL declarations are not supported on {} backend", self.capabilities.name),
                            p.span,
                        );
                    }
                }
                for param in &p.params {
                    self.check_param(param);
                }
                self.check_node(&p.block);
            }
            Node::FuncDecl(f) => {
                if f.is_forward {
                    if !self.capabilities.supports(LanguageFeature::ForwardExternal) {
                        self.add_error(
                            format!("FORWARD declarations are not supported on {} backend", self.capabilities.name),
                            f.span,
                        );
                    }
                }
                if f.is_external {
                    if !self.capabilities.supports(LanguageFeature::ForwardExternal) {
                        self.add_error(
                            format!("EXTERNAL declarations are not supported on {} backend", self.capabilities.name),
                            f.span,
                        );
                    }
                }
                for param in &f.params {
                    self.check_param(param);
                }
                self.check_node(&f.return_type);
                self.check_node(&f.block);
            }
            Node::IfStmt(s) => {
                self.check_node(&s.condition);
                self.check_node(&s.then_block);
                if let Some(else_block) = &s.else_block { self.check_node(else_block); }
            }
            Node::WhileStmt(s) => {
                self.check_node(&s.condition);
                self.check_node(&s.body);
            }
            Node::ForStmt(s) => {
                self.check_node(&s.start_expr);
                self.check_node(&s.end_expr);
                self.check_node(&s.body);
            }
            Node::ForInStmt(s) => {
                self.check_node(&s.collection_expr);
                self.check_node(&s.body);
            }
            Node::RepeatStmt(s) => {
                for stmt in &s.statements { self.check_node(stmt); }
                self.check_node(&s.condition);
            }
            Node::CaseStmt(s) => {
                self.check_node(&s.expr);
                for case in &s.cases {
                    for value in &case.values { self.check_node(value); }
                    self.check_node(&case.statement);
                }
                if let Some(else_branch) = &s.else_branch { self.check_node(else_branch); }
            }
            Node::TryStmt(s) => {
                for stmt in &s.try_block { self.check_node(stmt); }
                if let Some(except_block) = &s.except_block {
                    for stmt in except_block { self.check_node(stmt); }
                }
                if let Some(finally_block) = &s.finally_block {
                    for stmt in finally_block { self.check_node(stmt); }
                }
                for handler in &s.exception_handlers {
                    self.check_node(&handler.exception_type);
                    self.check_node(&handler.handler);
                }
                if let Some(else_clause) = &s.exception_else { self.check_node(else_clause); }
            }
            Node::WithStmt(s) => {
                for expr in &s.records { self.check_node(expr); }
                self.check_node(&s.statement);
            }
            Node::AssignStmt(s) => {
                self.check_node(&s.target);
                self.check_node(&s.value);
            }
            Node::CallStmt(s) => {
                for arg in &s.args { self.check_node(arg); }
            }
            Node::BinaryExpr(e) => {
                self.check_node(&e.left);
                self.check_node(&e.right);
            }
            Node::UnaryExpr(e) => {
                self.check_node(&e.expr);
            }
            Node::CallExpr(e) => {
                for arg in &e.args { self.check_node(arg); }
            }
            Node::IndexExpr(e) => {
                self.check_node(&e.array);
                self.check_node(&e.index);
            }
            Node::FieldExpr(e) => {
                self.check_node(&e.record);
            }
            Node::DerefExpr(e) => {
                self.check_node(&e.pointer);
            }
            _ => {}
        }
    }
    
    /// Check parameter for unsupported features
    fn check_param(&mut self, param: &ast::Param) {
        use ast::ParamType;
        match param.param_type {
            ParamType::ConstRef => {
                if !self.capabilities.supports(LanguageFeature::ConstRef) {
                    self.add_error(
                        format!("CONSTREF parameters are not supported on {} backend", self.capabilities.name),
                        param.span,
                    );
                }
            }
            ParamType::Out => {
                if !self.capabilities.supports(LanguageFeature::OutParams) {
                    self.add_error(
                        format!("OUT parameters are not supported on {} backend", self.capabilities.name),
                        param.span,
                    );
                }
            }
            _ => {}
        }
        if param.default_value.is_some() {
            if !self.capabilities.supports(LanguageFeature::DefaultParams) {
                self.add_error(
                    format!("Default parameter values are not supported on {} backend", self.capabilities.name),
                    param.span,
                );
            }
        }
        // Note: param.param_type_node is a Node, but we've already checked the ParamType enum above
    }
    
    /// Add an error diagnostic
    fn add_error(&mut self, message: String, span: Span) {
        let diag = Diagnostic::new(ErrorSeverity::Error, message, span)
            .with_file(self.filename.clone().unwrap_or_else(|| "unknown".to_string()));
        self.diagnostics.push(diag);
    }
}

/// Get human-readable name for a feature
fn feature_name(feature: LanguageFeature) -> &'static str {
    match feature {
        LanguageFeature::BasicTypes => "Basic Types",
        LanguageFeature::Arrays => "Arrays",
        LanguageFeature::DynamicArrays => "Dynamic Arrays",
        LanguageFeature::Records => "Records",
        LanguageFeature::Procedures => "Procedures",
        LanguageFeature::ControlFlow => "Control Flow",
        LanguageFeature::Sets => "Sets",
        LanguageFeature::Strings => "Strings",
        LanguageFeature::VariantRecords => "Variant Records",
        LanguageFeature::EnumeratedTypes => "Enumerated Types",
        LanguageFeature::Pointers => "Pointers",
        LanguageFeature::ProceduralTypes => "Procedural Types",
        LanguageFeature::FileTypes => "File Types",
        LanguageFeature::Classes => "Classes",
        LanguageFeature::Interfaces => "Interfaces",
        LanguageFeature::Properties => "Properties",
        LanguageFeature::OperatorOverloading => "Operator Overloading",
        LanguageFeature::MethodPointers => "Method Pointers",
        LanguageFeature::Generics => "Generics",
        LanguageFeature::AnonymousFunctions => "Anonymous Functions",
        LanguageFeature::NestedRoutines => "Nested Routines",
        LanguageFeature::ExceptionHandling => "Exception Handling",
        LanguageFeature::WithStatement => "WITH Statement",
        LanguageFeature::GotoLabels => "GOTO/Labels",
        LanguageFeature::InlineAssembly => "Inline Assembly",
        LanguageFeature::ForInLoops => "For..In Loops",
        LanguageFeature::ThreadVar => "THREADVAR",
        LanguageFeature::ConstRef => "CONSTREF",
        LanguageFeature::OutParams => "OUT Parameters",
        LanguageFeature::Resourcestring => "RESOURCESTRING",
        LanguageFeature::Absolute => "ABSOLUTE",
        LanguageFeature::DefaultParams => "Default Parameters",
        LanguageFeature::ForwardExternal => "FORWARD/EXTERNAL",
        LanguageFeature::ClassMethods => "Class Methods",
        LanguageFeature::ClassProperties => "Class Properties",
        LanguageFeature::ClassVariables => "Class Variables",
        LanguageFeature::ClassHelpers => "Class Helpers",
        LanguageFeature::NestedClasses => "Nested Classes",
        LanguageFeature::ReferenceCounting => "Reference Counting",
        LanguageFeature::GarbageCollection => "Garbage Collection",
        LanguageFeature::Multithreading => "Multithreading",
        LanguageFeature::DynamicLinking => "Dynamic Linking",
    }
}

