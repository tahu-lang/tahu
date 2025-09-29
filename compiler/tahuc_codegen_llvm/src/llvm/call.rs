pub enum CallConvention {
    C,
    FastCall,
    ColdCall,

    X86StdCall,
    X86ThisCall,

    SysVCall,
    Win64Call
}

impl CallConvention {
    pub fn as_llvm_call_conv(self) -> u32 {
        match self {
            CallConvention::C => 0,
            CallConvention::FastCall => 8,
            CallConvention::ColdCall => 9,
            CallConvention::X86StdCall => 64,
            CallConvention::X86ThisCall => 70,
            CallConvention::SysVCall => 78,
            CallConvention::Win64Call => 79,
        }
    }
}