//! SuperPascal Runtime Specification
//!
//! This crate defines runtime contracts, ABI specifications, and helper
//! function signatures for all target platforms. It serves as a specification
//! for what the runtime must provide, not an implementation.

/// Represents a target platform
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TargetPlatform {
    /// ZealZ80 - Zilog Z80 @ 10 MHz
    ZealZ80,
    /// Intel8051 - Intel 8051 microcontroller
    Intel8051,
    /// CommanderX16 - WDC 65C02 @ 8 MHz
    CommanderX16,
    /// Foenix65C816 - WDC W65C816S @ 6.29 MHz
    Foenix65C816,
    /// FoenixA2560M - MC68LC060 @ 66 MHz
    FoenixA2560M,
    /// RaspberryPi5 - ARM Cortex-A76 @ 2.4 GHz
    RaspberryPi5,
}

/// Represents a calling convention
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CallingConvention {
    /// Pascal-style: callee cleans stack
    Pascal,
    /// C-style: caller cleans stack
    C,
    /// Register-based: parameters in registers
    Register,
    /// Stack-based: all parameters on stack
    Stack,
    /// Mixed: some in registers, overflow on stack
    Mixed,
}

/// Represents a data type size and alignment
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TypeLayout {
    pub size: usize,
    pub alignment: usize,
}

impl TypeLayout {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }
}

/// Represents a register used for a specific purpose
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RegisterRole {
    /// Return value register (8-bit)
    Return8,
    /// Return value register (16-bit)
    Return16,
    /// Return value register (32-bit)
    Return32,
    /// Return value register (64-bit)
    Return64,
    /// Frame pointer
    FramePointer,
    /// Stack pointer
    StackPointer,
    /// Exception frame pointer
    ExceptionFrame,
    /// Parameter register (indexed)
    Parameter(usize),
    /// Scratch register (indexed)
    Scratch(usize),
}

/// Platform-specific ABI specification
#[derive(Debug, Clone)]
pub struct ABI {
    pub platform: TargetPlatform,
    pub calling_convention: CallingConvention,
    pub register_roles: Vec<(RegisterRole, String)>, // (role, register_name)
    pub stack_growth: StackGrowth,
    pub frame_pointer: Option<String>,
    pub return_registers: ReturnRegisters,
    pub parameter_passing: ParameterPassing,
}

/// Stack growth direction
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StackGrowth {
    /// Stack grows downward (toward smaller addresses)
    Downward,
    /// Stack grows upward (toward larger addresses)
    Upward,
}

/// Return value register specification
#[derive(Debug, Clone)]
pub struct ReturnRegisters {
    pub byte: Option<String>,      // 8-bit return
    pub word: Option<String>,      // 16-bit return
    pub dword: Option<String>,     // 32-bit return
    pub qword: Option<String>,     // 64-bit return
    pub pointer: Option<String>,    // Pointer return
    pub struct_small: Option<String>, // Small struct return (fits in register)
    pub struct_large: Option<String>,  // Large struct return (via pointer)
}

impl ReturnRegisters {
    pub fn new() -> Self {
        Self {
            byte: None,
            word: None,
            dword: None,
            qword: None,
            pointer: None,
            struct_small: None,
            struct_large: None,
        }
    }
}

impl Default for ReturnRegisters {
    fn default() -> Self {
        Self::new()
    }
}

/// Parameter passing specification
#[derive(Debug, Clone)]
pub struct ParameterPassing {
    pub register_count: usize,    // Number of parameter registers
    pub registers: Vec<String>,    // Register names for parameters
    pub stack_alignment: usize,    // Stack alignment for parameters
    pub stack_offset: usize,       // Offset from frame pointer to first stack parameter
}

impl ParameterPassing {
    pub fn new() -> Self {
        Self {
            register_count: 0,
            registers: vec![],
            stack_alignment: 1,
            stack_offset: 0,
        }
    }
}

impl Default for ParameterPassing {
    fn default() -> Self {
        Self::new()
    }
}

/// Runtime helper function signature
#[derive(Debug, Clone)]
pub struct RuntimeFunction {
    pub name: String,
    pub signature: FunctionSignature,
    pub platform: Option<TargetPlatform>, // None = available on all platforms
}

/// Function signature for runtime helpers
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub return_type: Option<TypeLayout>,
    pub parameters: Vec<(String, TypeLayout)>, // (name, type)
    pub calling_convention: CallingConvention,
}

impl ABI {
    /// Create a new ABI specification
    pub fn new(platform: TargetPlatform) -> Self {
        Self {
            platform,
            calling_convention: CallingConvention::Pascal, // Default
            register_roles: vec![],
            stack_growth: StackGrowth::Downward, // Default
            frame_pointer: None,
            return_registers: ReturnRegisters::new(),
            parameter_passing: ParameterPassing::new(),
        }
    }

    /// Get register name for a specific role
    pub fn get_register(&self, role: RegisterRole) -> Option<&String> {
        self.register_roles
            .iter()
            .find(|(r, _)| *r == role)
            .map(|(_, name)| name)
    }
}

/// Runtime helper functions that must be provided
pub struct RuntimeSpec {
    pub platform: TargetPlatform,
    pub functions: Vec<RuntimeFunction>,
}

impl RuntimeSpec {
    pub fn new(platform: TargetPlatform) -> Self {
        Self {
            platform,
            functions: vec![],
        }
    }

    pub fn add_function(&mut self, func: RuntimeFunction) {
        self.functions.push(func);
    }
}

// Platform-specific ABI definitions

/// Get ABI specification for ZealZ80
pub fn zealz80_abi() -> ABI {
    let mut abi = ABI::new(TargetPlatform::ZealZ80);
    abi.calling_convention = CallingConvention::Pascal;
    abi.stack_growth = StackGrowth::Downward;
    abi.frame_pointer = Some("IX".to_string());
    abi.return_registers.byte = Some("A".to_string());
    abi.return_registers.word = Some("HL".to_string());
    abi.return_registers.pointer = Some("HL".to_string());
    
    abi.register_roles.push((RegisterRole::FramePointer, "IX".to_string()));
    abi.register_roles.push((RegisterRole::StackPointer, "SP".to_string()));
    abi.register_roles.push((RegisterRole::ExceptionFrame, "IY".to_string()));
    abi.register_roles.push((RegisterRole::Return8, "A".to_string()));
    abi.register_roles.push((RegisterRole::Return16, "HL".to_string()));
    
    abi.parameter_passing.register_count = 0; // Stack-based for Z80
    abi.parameter_passing.stack_alignment = 2;
    abi.parameter_passing.stack_offset = 2; // Skip return address
    
    abi
}

/// Get ABI specification for Intel8051 (based on Turbo51)
pub fn intel8051_abi() -> ABI {
    let mut abi = ABI::new(TargetPlatform::Intel8051);
    abi.calling_convention = CallingConvention::Pascal;
    abi.stack_growth = StackGrowth::Downward;
    abi.frame_pointer = None; // No frame pointer (static allocation)
    abi.return_registers.byte = Some("A".to_string()); // Accumulator
    abi.return_registers.word = Some("R5R4R3R2".to_string()); // Register set
    abi.return_registers.pointer = Some("R0".to_string()); // For DATA/IDATA
    // DPTR for CODE/XDATA pointers
    
    abi.register_roles.push((RegisterRole::Return8, "A".to_string()));
    abi.register_roles.push((RegisterRole::Return16, "R5R4R3R2".to_string()));
    abi.register_roles.push((RegisterRole::StackPointer, "SP".to_string()));
    
    abi.parameter_passing.register_count = 0; // Parameters stored as local variables
    abi.parameter_passing.stack_alignment = 1;
    abi.parameter_passing.stack_offset = 0; // No stack for normal procedures
    
    abi
}

/// Get ABI specification for CommanderX16 (65C02)
pub fn commanderx16_abi() -> ABI {
    let mut abi = ABI::new(TargetPlatform::CommanderX16);
    abi.calling_convention = CallingConvention::Stack;
    abi.stack_growth = StackGrowth::Downward;
    abi.frame_pointer = None; // Stack-based
    abi.return_registers.byte = Some("A".to_string());
    abi.return_registers.word = Some("AX".to_string()); // A + X for 16-bit
    
    abi.register_roles.push((RegisterRole::Return8, "A".to_string()));
    abi.register_roles.push((RegisterRole::Return16, "AX".to_string()));
    abi.register_roles.push((RegisterRole::StackPointer, "SP".to_string()));
    
    abi.parameter_passing.register_count = 0; // Stack-based
    abi.parameter_passing.stack_alignment = 1;
    abi.parameter_passing.stack_offset = 1; // Skip return address
    
    abi
}

/// Get ABI specification for a platform
pub fn get_abi(platform: TargetPlatform) -> ABI {
    match platform {
        TargetPlatform::ZealZ80 => zealz80_abi(),
        TargetPlatform::Intel8051 => intel8051_abi(),
        TargetPlatform::CommanderX16 => commanderx16_abi(),
        TargetPlatform::Foenix65C816 => {
            // TODO: Define 65C816 ABI
            ABI::new(platform)
        }
        TargetPlatform::FoenixA2560M => {
            // TODO: Define m68k ABI (reference FPC)
            ABI::new(platform)
        }
        TargetPlatform::RaspberryPi5 => {
            // TODO: Define ARM64 ABI (AAPCS64)
            ABI::new(platform)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_target_platform() {
        assert_eq!(TargetPlatform::ZealZ80, TargetPlatform::ZealZ80);
        assert_ne!(TargetPlatform::ZealZ80, TargetPlatform::Intel8051);
    }

    #[test]
    fn test_calling_convention() {
        assert_eq!(CallingConvention::Pascal, CallingConvention::Pascal);
        assert_ne!(CallingConvention::Pascal, CallingConvention::C);
    }

    #[test]
    fn test_type_layout() {
        let layout = TypeLayout::new(2, 2);
        assert_eq!(layout.size, 2);
        assert_eq!(layout.alignment, 2);
    }

    #[test]
    fn test_zealz80_abi() {
        let abi = zealz80_abi();
        assert_eq!(abi.platform, TargetPlatform::ZealZ80);
        assert_eq!(abi.calling_convention, CallingConvention::Pascal);
        assert_eq!(abi.stack_growth, StackGrowth::Downward);
        assert_eq!(abi.frame_pointer, Some("IX".to_string()));
        assert_eq!(abi.return_registers.byte, Some("A".to_string()));
        assert_eq!(abi.return_registers.word, Some("HL".to_string()));
    }

    #[test]
    fn test_intel8051_abi() {
        let abi = intel8051_abi();
        assert_eq!(abi.platform, TargetPlatform::Intel8051);
        assert_eq!(abi.calling_convention, CallingConvention::Pascal);
        assert_eq!(abi.return_registers.byte, Some("A".to_string()));
        assert_eq!(abi.return_registers.word, Some("R5R4R3R2".to_string()));
    }

    #[test]
    fn test_commanderx16_abi() {
        let abi = commanderx16_abi();
        assert_eq!(abi.platform, TargetPlatform::CommanderX16);
        assert_eq!(abi.calling_convention, CallingConvention::Stack);
        assert_eq!(abi.return_registers.byte, Some("A".to_string()));
    }

    #[test]
    fn test_get_register() {
        let abi = zealz80_abi();
        assert_eq!(abi.get_register(RegisterRole::FramePointer), Some(&"IX".to_string()));
        assert_eq!(abi.get_register(RegisterRole::Return16), Some(&"HL".to_string()));
    }

    #[test]
    fn test_get_abi() {
        let z80_abi = get_abi(TargetPlatform::ZealZ80);
        assert_eq!(z80_abi.platform, TargetPlatform::ZealZ80);
        
        let i8051_abi = get_abi(TargetPlatform::Intel8051);
        assert_eq!(i8051_abi.platform, TargetPlatform::Intel8051);
    }

    #[test]
    fn test_runtime_spec() {
        let mut spec = RuntimeSpec::new(TargetPlatform::ZealZ80);
        let func = RuntimeFunction {
            name: "test_func".to_string(),
            signature: FunctionSignature {
                return_type: Some(TypeLayout::new(2, 2)),
                parameters: vec![("x".to_string(), TypeLayout::new(2, 2))],
                calling_convention: CallingConvention::Pascal,
            },
            platform: Some(TargetPlatform::ZealZ80),
        };
        spec.add_function(func);
        assert_eq!(spec.functions.len(), 1);
    }
}
