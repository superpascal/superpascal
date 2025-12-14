//! SuperPascal Intermediate Representation (IR)
//!
//! This crate defines a platform-agnostic intermediate representation
//! for SuperPascal programs. The IR is designed to be:
//! - Simple and linear (IR1)
//! - Platform-independent
//! - Easy to optimize
//! - Easy to translate to target assembly

use ast::Node;
use tokens::Span;
use types::Type;

/// Represents an IR value (immediate, register, memory, temporary)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Value {
    /// Immediate constant value
    Immediate(i32),
    /// Register (platform-specific, but abstracted here)
    Register(String),
    /// Memory location (address)
    Memory { base: String, offset: i32 },
    /// Temporary value (SSA-style)
    Temp(usize),
    /// Label reference
    Label(String),
}

/// IR instruction opcodes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
    // Data movement
    Mov,  // MOV dst, src
    // Arithmetic
    Add,  // ADD dst, src1, src2
    Sub,  // SUB dst, src1, src2
    Mul,  // MUL dst, src1, src2
    Div,  // DIV dst, src1, src2
    Mod,  // MOD dst, src1, src2
    // Comparison
    Cmp,  // CMP src1, src2 (sets condition flags)
    // Control flow
    Jump,   // JUMP label
    CJump,  // CJUMP condition, label_true, label_false
    Call,   // CALL function, result
    Ret,    // RET value (optional)
    // Memory operations
    Load,   // LOAD dst, src (load from memory)
    Store,  // STORE dst, src (store to memory)
    // Stack operations
    Push,   // PUSH src
    Pop,    // POP dst
}

/// Condition codes for conditional jumps
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=
}

/// Represents a single IR instruction
#[derive(Debug, Clone, PartialEq)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operands: Vec<Value>,
    pub span: Option<Span>, // Source location for debugging
}

impl Instruction {
    pub fn new(opcode: Opcode, operands: Vec<Value>) -> Self {
        Self {
            opcode,
            operands,
            span: None,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
}

/// Represents a basic block in the IR
#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub label: String,
    pub instructions: Vec<Instruction>,
    pub successors: Vec<String>, // Labels of successor blocks
}

impl BasicBlock {
    pub fn new(label: String) -> Self {
        Self {
            label,
            instructions: vec![],
            successors: vec![],
        }
    }

    pub fn add_instruction(&mut self, inst: Instruction) {
        self.instructions.push(inst);
    }

    pub fn add_successor(&mut self, label: String) {
        if !self.successors.contains(&label) {
            self.successors.push(label);
        }
    }
}

/// Represents a complete IR function/procedure
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>, // (name, type)
    pub return_type: Option<Type>,
    pub blocks: Vec<BasicBlock>,
    pub entry_block: String, // Label of entry block
}

impl Function {
    pub fn new(name: String, return_type: Option<Type>) -> Self {
        let entry_label = format!("{}_entry", name);
        let entry_block = BasicBlock::new(entry_label.clone());
        
        Self {
            name,
            params: vec![],
            return_type,
            blocks: vec![entry_block],
            entry_block: entry_label,
        }
    }

    pub fn add_block(&mut self, block: BasicBlock) {
        self.blocks.push(block);
    }

    pub fn get_block_mut(&mut self, label: &str) -> Option<&mut BasicBlock> {
        self.blocks.iter_mut().find(|b| b.label == label)
    }
}

/// Represents a complete IR program
#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: Vec<(String, Type)>, // (name, type)
}

impl Program {
    pub fn new() -> Self {
        Self {
            functions: vec![],
            globals: vec![],
        }
    }

    pub fn add_function(&mut self, func: Function) {
        self.functions.push(func);
    }
}

impl Default for Program {
    fn default() -> Self {
        Self::new()
    }
}

/// IR builder for constructing IR from AST
pub struct IRBuilder {
    program: Program,
    current_function: Option<Function>,
    temp_counter: usize,
    label_counter: usize,
}

impl IRBuilder {
    pub fn new() -> Self {
        Self {
            program: Program::new(),
            current_function: None,
            temp_counter: 0,
            label_counter: 0,
        }
    }

    /// Generate a new temporary value
    pub fn new_temp(&mut self) -> Value {
        let temp = self.temp_counter;
        self.temp_counter += 1;
        Value::Temp(temp)
    }

    /// Generate a new label
    pub fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    /// Start building a new function
    pub fn start_function(&mut self, name: String, return_type: Option<Type>) {
        self.current_function = Some(Function::new(name, return_type));
    }

    /// Finish the current function and add it to the program
    pub fn finish_function(&mut self) {
        if let Some(func) = self.current_function.take() {
            self.program.add_function(func);
        }
    }

    /// Get the current function (mutable)
    pub fn current_function_mut(&mut self) -> Option<&mut Function> {
        self.current_function.as_mut()
    }

    /// Build IR from AST
    pub fn build(&mut self, _ast: &Node) -> Program {
        // TODO: Implement AST to IR translation
        // For now, return empty program
        self.program.clone()
    }

    /// Get the built program
    pub fn into_program(self) -> Program {
        self.program
    }
}

impl Default for IRBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokens::Span;

    // Value tests
    #[test]
    fn test_value_immediate() {
        let imm = Value::Immediate(42);
        assert_eq!(imm, Value::Immediate(42));
        assert_ne!(imm, Value::Immediate(43));
    }

    #[test]
    fn test_value_register() {
        let reg = Value::Register("r0".to_string());
        assert_eq!(reg, Value::Register("r0".to_string()));
        assert_ne!(reg, Value::Register("r1".to_string()));
    }

    #[test]
    fn test_value_memory() {
        let mem = Value::Memory { base: "sp".to_string(), offset: 4 };
        assert_eq!(mem, Value::Memory { base: "sp".to_string(), offset: 4 });
        assert_ne!(mem, Value::Memory { base: "sp".to_string(), offset: 8 });
    }

    #[test]
    fn test_value_temp() {
        let temp = Value::Temp(0);
        assert_eq!(temp, Value::Temp(0));
        assert_ne!(temp, Value::Temp(1));
    }

    #[test]
    fn test_value_label() {
        let label = Value::Label("loop".to_string());
        assert_eq!(label, Value::Label("loop".to_string()));
        assert_ne!(label, Value::Label("end".to_string()));
    }

    // Opcode tests
    #[test]
    fn test_opcode_equality() {
        assert_eq!(Opcode::Add, Opcode::Add);
        assert_ne!(Opcode::Add, Opcode::Sub);
        assert_eq!(Opcode::Mov, Opcode::Mov);
        assert_eq!(Opcode::Jump, Opcode::Jump);
        assert_eq!(Opcode::Call, Opcode::Call);
        assert_eq!(Opcode::Ret, Opcode::Ret);
    }

    // Condition tests
    #[test]
    fn test_condition_equality() {
        assert_eq!(Condition::Equal, Condition::Equal);
        assert_ne!(Condition::Equal, Condition::NotEqual);
        assert_eq!(Condition::Less, Condition::Less);
        assert_eq!(Condition::GreaterEqual, Condition::GreaterEqual);
    }

    // Instruction tests
    #[test]
    fn test_instruction_creation() {
        let inst = Instruction::new(
            Opcode::Add,
            vec![
                Value::Temp(0),
                Value::Immediate(1),
                Value::Immediate(2),
            ],
        );
        
        assert_eq!(inst.opcode, Opcode::Add);
        assert_eq!(inst.operands.len(), 3);
        assert_eq!(inst.span, None);
    }

    #[test]
    fn test_instruction_with_span() {
        let span = Span::new(0, 10, 1, 1);
        let inst = Instruction::new(Opcode::Mov, vec![
            Value::Temp(0),
            Value::Immediate(42),
        ]).with_span(span);
        
        assert_eq!(inst.opcode, Opcode::Mov);
        assert_eq!(inst.span, Some(span));
    }

    #[test]
    fn test_instruction_all_opcodes() {
        let ops = vec![
            Opcode::Mov, Opcode::Add, Opcode::Sub, Opcode::Mul,
            Opcode::Div, Opcode::Mod, Opcode::Cmp, Opcode::Jump,
            Opcode::CJump, Opcode::Call, Opcode::Ret, Opcode::Load,
            Opcode::Store, Opcode::Push, Opcode::Pop,
        ];
        
        for op in &ops {
            let inst = Instruction::new(op.clone(), vec![]);
            assert_eq!(inst.opcode, *op);
        }
    }

    // BasicBlock tests
    #[test]
    fn test_basic_block_creation() {
        let block = BasicBlock::new("label1".to_string());
        assert_eq!(block.label, "label1");
        assert_eq!(block.instructions.len(), 0);
        assert_eq!(block.successors.len(), 0);
    }

    #[test]
    fn test_basic_block_add_instruction() {
        let mut block = BasicBlock::new("label1".to_string());
        block.add_instruction(Instruction::new(Opcode::Mov, vec![
            Value::Temp(0),
            Value::Immediate(42),
        ]));
        block.add_instruction(Instruction::new(Opcode::Add, vec![
            Value::Temp(1),
            Value::Temp(0),
            Value::Immediate(1),
        ]));
        
        assert_eq!(block.instructions.len(), 2);
        assert_eq!(block.instructions[0].opcode, Opcode::Mov);
        assert_eq!(block.instructions[1].opcode, Opcode::Add);
    }

    #[test]
    fn test_basic_block_add_successor() {
        let mut block = BasicBlock::new("label1".to_string());
        block.add_successor("label2".to_string());
        block.add_successor("label3".to_string());
        
        assert_eq!(block.successors.len(), 2);
        assert!(block.successors.contains(&"label2".to_string()));
        assert!(block.successors.contains(&"label3".to_string()));
    }

    #[test]
    fn test_basic_block_duplicate_successor() {
        let mut block = BasicBlock::new("label1".to_string());
        block.add_successor("label2".to_string());
        block.add_successor("label2".to_string()); // Duplicate
        
        assert_eq!(block.successors.len(), 1); // Should not add duplicate
    }

    // Function tests
    #[test]
    fn test_function_creation() {
        let func = Function::new("test".to_string(), Some(Type::integer()));
        assert_eq!(func.name, "test");
        assert_eq!(func.blocks.len(), 1); // Entry block
        assert_eq!(func.entry_block, "test_entry");
        assert_eq!(func.params.len(), 0);
        assert_eq!(func.return_type, Some(Type::integer()));
    }

    #[test]
    fn test_function_procedure() {
        let func = Function::new("proc".to_string(), None);
        assert_eq!(func.name, "proc");
        assert_eq!(func.return_type, None); // Procedure has no return type
    }

    #[test]
    fn test_function_add_block() {
        let mut func = Function::new("test".to_string(), Some(Type::integer()));
        let block1 = BasicBlock::new("block1".to_string());
        let block2 = BasicBlock::new("block2".to_string());
        func.add_block(block1);
        func.add_block(block2);
        
        assert_eq!(func.blocks.len(), 3); // Entry + 2 blocks
    }

    #[test]
    fn test_function_get_block_mut() {
        let mut func = Function::new("test".to_string(), None);
        let mut block = BasicBlock::new("block1".to_string());
        block.add_instruction(Instruction::new(Opcode::Mov, vec![]));
        func.add_block(block);
        
        if let Some(entry_block) = func.get_block_mut("test_entry") {
            entry_block.add_instruction(Instruction::new(Opcode::Ret, vec![]));
            assert_eq!(entry_block.instructions.len(), 1);
        } else {
            panic!("Entry block not found");
        }
        
        if let Some(block) = func.get_block_mut("block1") {
            assert_eq!(block.instructions.len(), 1);
        } else {
            panic!("Block1 not found");
        }
        
        assert!(func.get_block_mut("nonexistent").is_none());
    }

    // Program tests
    #[test]
    fn test_program_creation() {
        let program = Program::new();
        assert_eq!(program.functions.len(), 0);
        assert_eq!(program.globals.len(), 0);
    }

    #[test]
    fn test_program_default() {
        let program = Program::default();
        assert_eq!(program.functions.len(), 0);
        assert_eq!(program.globals.len(), 0);
    }

    #[test]
    fn test_program_add_function() {
        let mut program = Program::new();
        let func1 = Function::new("func1".to_string(), Some(Type::integer()));
        let func2 = Function::new("func2".to_string(), None);
        program.add_function(func1);
        program.add_function(func2);
        
        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].name, "func1");
        assert_eq!(program.functions[1].name, "func2");
    }

    // IRBuilder tests
    #[test]
    fn test_ir_builder_new() {
        let builder = IRBuilder::new();
        assert_eq!(builder.temp_counter, 0);
        assert_eq!(builder.label_counter, 0);
        assert!(builder.current_function.is_none());
    }

    #[test]
    fn test_ir_builder_default() {
        let builder = IRBuilder::default();
        assert_eq!(builder.temp_counter, 0);
    }

    #[test]
    fn test_ir_builder_new_temp() {
        let mut builder = IRBuilder::new();
        let temp1 = builder.new_temp();
        let temp2 = builder.new_temp();
        let temp3 = builder.new_temp();
        
        assert_eq!(temp1, Value::Temp(0));
        assert_eq!(temp2, Value::Temp(1));
        assert_eq!(temp3, Value::Temp(2));
    }

    #[test]
    fn test_ir_builder_new_label() {
        let mut builder = IRBuilder::new();
        let label1 = builder.new_label("loop");
        let label2 = builder.new_label("loop");
        let label3 = builder.new_label("end");
        
        assert_eq!(label1, "loop_0");
        assert_eq!(label2, "loop_1");
        assert_eq!(label3, "end_2");
    }

    #[test]
    fn test_ir_builder_start_function() {
        let mut builder = IRBuilder::new();
        builder.start_function("test".to_string(), Some(Type::integer()));
        
        assert!(builder.current_function.is_some());
        let func = builder.current_function.as_ref().unwrap();
        assert_eq!(func.name, "test");
        assert_eq!(func.return_type, Some(Type::integer()));
    }

    #[test]
    fn test_ir_builder_finish_function() {
        let mut builder = IRBuilder::new();
        builder.start_function("test".to_string(), None);
        builder.finish_function();
        
        assert!(builder.current_function.is_none());
        assert_eq!(builder.program.functions.len(), 1);
        assert_eq!(builder.program.functions[0].name, "test");
    }

    #[test]
    fn test_ir_builder_current_function_mut() {
        let mut builder = IRBuilder::new();
        assert!(builder.current_function_mut().is_none());
        
        builder.start_function("test".to_string(), None);
        if let Some(func) = builder.current_function_mut() {
            func.add_block(BasicBlock::new("block1".to_string()));
            assert_eq!(func.blocks.len(), 2); // Entry + block1
        } else {
            panic!("Function should exist");
        }
    }

    #[test]
    fn test_ir_builder_into_program() {
        let mut builder = IRBuilder::new();
        builder.start_function("func1".to_string(), None);
        builder.finish_function();
        builder.start_function("func2".to_string(), Some(Type::integer()));
        builder.finish_function();
        
        let program = builder.into_program();
        assert_eq!(program.functions.len(), 2);
    }

    // Integration tests
    #[test]
    fn test_complete_ir_program() {
        let mut program = Program::new();
        
        // Create main function
        let mut main_func = Function::new("main".to_string(), None);
        let mut entry_block = BasicBlock::new("main_entry".to_string());
        entry_block.add_instruction(Instruction::new(Opcode::Mov, vec![
            Value::Temp(0),
            Value::Immediate(42),
        ]));
        entry_block.add_instruction(Instruction::new(Opcode::Ret, vec![]));
        main_func.add_block(entry_block);
        program.add_function(main_func);
        
        // Create helper function
        let mut helper_func = Function::new("helper".to_string(), Some(Type::integer()));
        let mut helper_entry = BasicBlock::new("helper_entry".to_string());
        helper_entry.add_instruction(Instruction::new(Opcode::Add, vec![
            Value::Temp(0),
            Value::Temp(1),
            Value::Immediate(1),
        ]));
        helper_entry.add_instruction(Instruction::new(Opcode::Ret, vec![
            Value::Temp(0),
        ]));
        helper_func.add_block(helper_entry);
        program.add_function(helper_func);
        
        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].name, "main");
        assert_eq!(program.functions[1].name, "helper");
    }

    #[test]
    fn test_ir_builder_complete_workflow() {
        let mut builder = IRBuilder::new();
        
        // Build main function
        builder.start_function("main".to_string(), None);
        let temp = builder.new_temp(); // Generate temp before borrowing func
        if let Some(func) = builder.current_function_mut() {
            if let Some(entry) = func.get_block_mut("main_entry") {
                entry.add_instruction(Instruction::new(Opcode::Mov, vec![
                    temp.clone(),
                    Value::Immediate(10),
                ]));
                entry.add_instruction(Instruction::new(Opcode::Ret, vec![]));
            }
        }
        builder.finish_function();
        
        // Build helper function
        builder.start_function("helper".to_string(), Some(Type::integer()));
        builder.finish_function();
        
        let program = builder.into_program();
        assert_eq!(program.functions.len(), 2);
        assert_eq!(program.functions[0].blocks[0].instructions.len(), 2);
    }
}
